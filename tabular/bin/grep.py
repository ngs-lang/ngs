#!/usr/bin/env python

import argparse
import re
import sys

import tabular.tabular as tabular

parser = argparse.ArgumentParser(description='GREP specified columns (default all) of tabular data')
parser.add_argument('-c', metavar='COLUMN_NAME_RE', type=str, dest='column_re', default=None)
parser.add_argument('-v', dest='invert', action='store_true')
parser.add_argument('re')
parser.set_defaults(invert=False)
args = parser.parse_args()

if args.column_re:
    column_re = re.compile(args.column_re)
else:
    column_re = None
value_re = re.compile(args.re)

r = tabular.Reader(sys.stdin)

all_columns = r.get_columns_names()
if column_re:
    columns = [c for c in all_columns if column_re.search(c)]
else:
    columns = all_columns

w = tabular.Writer(columns=columns)
for line in r:
    matching_columns = [c for c in columns if value_re.search(str(line[c]))]
    if matching_columns and not args.invert:
        w.write(**line)

w.close()
