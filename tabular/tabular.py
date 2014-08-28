#!/usr/bin/env python
# vim: ts=4 sw=4 et

"""
This is part of a POC project that deals with tabular data.
It's a shame that the fact that most of the data in shell is tabular
was ignored up until now. No, "awk '{print $3}'" is _not_ dealing with
tables. It deals with records.
"""

# TODO:
#  isatty() -> only output some records (N first, M last) and dots in the middle
#  not isatty() -> output immediately, not on close()

# WIP: lines_transformators

# Sample usage: ec2din.py | awk.py 'write(env=line["tags"]["env"], name=line["tags"]["Name"], status=line["tags"].get("status"), **line)' | select.py id,ip_address,env,name,status

from __future__ import print_function

import argparse
import fcntl
import json
import os
import struct
import sys
import termios

import colorama

from history import *

class TabularIO(object):
    def get_columns_names(self):
        return self.columns

def cell_to_str(v):
    if v is None:
        return ''
    if isinstance(v, list):
        return ','.join(map(cell_to_str, v))
    if isinstance(v, dict) and '$display' in v and '$value' in v:
        return cell_to_str(v['$display'])
    return str(v)

class ColumnRuleExcludeByName(object):
    def __init__(self, column_name):
        self.column_name = column_name
    def allow(self, column_name, value):
        return column_name != self.column_name


class Writer(TabularIO):
    def __init__(self, **kw):
        attrs = {
            'columns': [],
            'columns_rules': [],
            'display_columns': [],
            'lines_transformators': [],
            'version': 1
        }
        attrs.update(kw)
        for k, v in attrs.items():
            setattr(self, k, v)
        self.buf = []
        self.attrs = attrs

    def _object_data_attrs(self, obj):
        if hasattr(obj, '__class__'):
            class_name = obj.__class__.__module__ + "." + obj.__class__.__name__
        else:
            class_name = None

        # TODO: maybe move to rules
        attrs = []
        for a in dir(obj):
            if a.startswith('_'):
                continue
            v = getattr(obj, a)
            try:
                if hasattr(v, '__call__'):
                    continue
            except AttributeError:
                # Not sure
                continue
            ok = all([rule.allow(a, v) for rule in self.columns_rules])
            if not ok:
                continue
            attrs.append(a)

        return attrs


    def maybe_init(self, **kv_pairs):
        if not kv_pairs:
            return
        if not self.columns:
            self.columns = sorted(kv_pairs.keys())

    def maybe_init_from_obj(self, obj):
        if self.columns:
            return
        self.columns = sorted(self._object_data_attrs(obj))

    def write(self, **kv_pairs):
        for transformator in self.lines_transformators:
            transformator(kv_pairs)
        self.maybe_init(**kv_pairs)
        if kv_pairs:
            self.buf.append([kv_pairs[k] for k in self.columns])
            return

    def write_obj(self, obj):
        self.maybe_init_from_obj(obj)
        line = {c: getattr(obj, c) for c in self.columns}
        self.write(**line)

    # Sugar
    def write_objects_list(self, iterable):
        for obj in iterable:
            self.write_obj(obj)

    def _calc_widths_for_columns(self, columns):
        column_name_to_number = {c: self.columns.index(c) for c in self.columns}
        widths = [len(c) for c in columns]
        for line in self.buf:
            for i in range(0, len(columns)):
                widths[i] = max(widths[i], len(cell_to_str(self.serialize(line[column_name_to_number[columns[i]]]))))
        return widths

    # TODO: periodic, paginated output for console
    # TODO: un-uglify the code ? :)
    def close(self):
        stream = sys.stdout
        if stream.isatty():

            # Pick columns to display based on specified columns and display width - start
            # Columns detection "inspired by"
            # http://blog.taz.net.au/2012/04/09/getting-the-terminal-size-in-python/
            hw = struct.unpack('hh', fcntl.ioctl(stream, termios.TIOCGWINSZ, '1234'))
            terminal_cols = hw[1]
            display_columns = self.display_columns[:]
            column_name_to_number = {c: self.columns.index(c) for c in self.columns}
            while True:
                widths = self._calc_widths_for_columns(display_columns)
                widths_sum = reduce(int.__add__, widths, 0) + (len(widths) - 1) * 2  # 2 spaces between columns
                if widths_sum < terminal_cols:
                    break
                display_columns = display_columns[:-1]
            # Pick columns to display based on specified columns and display width - end

            print(colorama.Style.BRIGHT, end='')
            for i, w in enumerate(widths):
                print(("{0:"+str(w)+"}  ").format(str(display_columns[i])), end='')
            print(colorama.Style.RESET_ALL)
            # if len(display_columns) != len(self.columns):
            #     print("  ... {0}".format(' '.join(sorted(list(set(self.columns)-set(display_columns))))))
            for line in self.buf:
                for i, w in enumerate(widths):
                    print(("{0:"+str(w)+"}  ").format(cell_to_str(self.serialize(line[column_name_to_number[display_columns[i]]]))), end='')
                print()
        else:
            # TODO: Find some better way to detect whch attributes should be printed
            attrs = {a: getattr(self, a) for a in self.attrs if a != 'columns_rules' and a != 'lines_transformators'}
            print(json.dumps(attrs))
            for line in self.buf:
                print(json.dumps([self.serialize(x) for x in line]))

    # Wasteful serialization (happens twice), fix later
    # Visited should be set() but does not work with (some?) objects
    def serialize(self, x, visited=None):
        if visited is None:
            visited = []
        if x in visited:
            return '*RECURSION*'
        if isinstance(x, list):
            return [self.serialize(i, visited + [x]) for i in x]
        if isinstance(x, dict):
            return {k: self.serialize(v, visited + [x]) for k,v in x.items()}
        try:
            json.dumps(x)
        except TypeError:
            # Might turn out as bad heuristic
            for attr in 'id', 'name':
                if hasattr(x, attr):
                    return self.serialize(getattr(x, attr))
            data_attrs = self._object_data_attrs(x)
            # print(x.__class__.__name__)
            return {k: self.serialize(getattr(x, k, None), visited + [x]) for k in data_attrs}
        return x

# Warning: iterator is not safe, use only once!
class Reader(TabularIO):

    def __init__(self, stream):
        self.stream = stream
        meta = json.loads(self.stream.readline())
        self.meta = meta
        self.columns = meta['columns']

    def read(self):
        line = self.stream.readline()
        if line == '':
            raise StopIteration
        line_data = json.loads(line)
        ret = {c: line_data[i] for i, c in enumerate(self.columns)}
        return ret

    def __iter__(self):
        return self

    def next(self):
        return self.read()

def write_objects_list(iterable, **kw):
    w = Writer(**kw)
    w.write_objects_list(iterable)
    w.close()
