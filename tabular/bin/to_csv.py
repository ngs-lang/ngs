#!/usr/bin/env python
import sys
import csv
import tabular.tabular as tabular

r = tabular.Reader(sys.stdin)
w = csv.DictWriter(sys.stdout, r.get_columns_names())
w.writeheader()
for line in r:
    out = {k: tabular.cell_to_str(line[k]) for k in r.get_columns_names()}
    w.writerow(out)
