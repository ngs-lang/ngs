#!/usr/bin/env python

import argparse
import re
import sys

import tabular.tabular as tabular

parser = argparse.ArgumentParser(description='wannabe awk of tabular data')
parser.add_argument('code')
args = parser.parse_args()

r = tabular.Reader(sys.stdin)
w = tabular.Writer()

for NR, line in enumerate(r, 1):
    g = globals()
    g.update({
        'line': line,
        'NR': NR,
        'write': w.write,
    })
    g.update({'c_'+c: line[c] for c in r.get_columns_names()})
    exec args.code in g


w.close()

