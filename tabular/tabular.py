#!/usr/bin/env python
# vim: ts=4 sw=4 et

# TODO:
#  isatty() -> only output some records (N first, M last) and dots in the middle
#  not isatty() -> output immediately, not on close()

from __future__ import print_function

import argparse
import json
import sys

import colorama

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
    def __init__(self, columns=None, columns_rules=None, display_columns=None):
        self.columns = columns or []
        self.buf = []
        self.columns_rules = columns_rules or []
        self.display_columns = display_columns or self.columns

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

    def write(self, *__values, **kv_pairs):
        self.maybe_init(**kv_pairs)
        if __values:
            self.buf.append(__values)
            return
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

    def close(self):
        stream = sys.stdout
        if stream.isatty():
            column_name_to_number = {c: self.columns.index(c) for c in self.columns}
            widths = [len(c) for c in self.display_columns]
            for line in self.buf:
                for i in range(0, len(self.display_columns)):
                    widths[i] = max(widths[i], len(cell_to_str(self.serialize(line[column_name_to_number[self.display_columns[i]]]))))
            print(colorama.Style.BRIGHT, end='')
            for i, w in enumerate(widths):
                print(("{0:"+str(w)+"} ").format(str(self.display_columns[i])), end='')
            print(colorama.Style.RESET_ALL)
            if len(self.display_columns) != len(self.columns):
                print("  ... {0}".format(' '.join(sorted(list(set(self.columns)-set(self.display_columns))))))
            for line in self.buf:
                for i, w in enumerate(widths):
                    print(("{0:"+str(w)+"} ").format(cell_to_str(self.serialize(line[column_name_to_number[self.display_columns[i]]]))), end='')
                print()
        else:
            print(json.dumps({'version': 1, 'columns': self.columns}))
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
