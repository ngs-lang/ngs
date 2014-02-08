#!/usr/bin/env python
import sys
import tabular.tabular as tabular

columns = sys.argv[1].split(',')

r = tabular.Reader(sys.stdin)
w = tabular.Writer(*columns)
for line in r:
    w.write(**{c: line[c] for c in columns})
w.close()
