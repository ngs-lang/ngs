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

class Writer(TabularIO):
    def __init__(self, *columns):
        self.columns = columns
        self.buf = []

    def maybe_init(self, **kv_pairs):
        if not kv_pairs:
            return
        if not self.columns:
            self.columns = sorted(kv_pairs.keys())

    def write(self, *__values, **kv_pairs):
        self.maybe_init(**kv_pairs)
        if __values:
            self.buf.append(__values)
            return
        if kv_pairs:
            self.buf.append([kv_pairs[k] for k in self.columns])
            return

    def close(self):
        stream = sys.stdout
        if stream.isatty():
            widths = [len(c) for c in self.columns]
            for line in self.buf:
                for i in range(0, len(self.columns)):
                    widths[i] = max(widths[i], len(str(line[i])))
            print(colorama.Style.BRIGHT, end='')
            for i, w in enumerate(widths):
                print(("{0:"+str(w)+"} ").format(str(self.columns[i])), end='')
            print(colorama.Style.RESET_ALL)
            for line in self.buf:
                for i, w in enumerate(widths):
                    print(("{0:"+str(w)+"} ").format(str(line[i])), end='')
                print()
        else:
            print(json.dumps({'version': 1, 'columns': self.columns}))
            for line in self.buf:
                print(json.dumps(line))

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
