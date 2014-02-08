#!/usr/bin/env python

# TODO: support URLs: file://, http(s)://, s3?
# TODO: support recursive
# TODO: if top level is dir - show contents, as ls without -d does.

import argparse
import json
import os
import stat

from tabular.tabular import *

def stat_to_hash(s):
	return {k[3:]: getattr(s, k) for k in dir(s) if k.startswith('st_')}

def ls_location(location, writer):
	s = os.stat(location)
	h = stat_to_hash(s)
	h['name'] = location
	# print(json.dumps(h))
	w.write(**h)



parser = argparse.ArgumentParser(description='List locations')
parser.add_argument('locations', metavar='LOCATION', type=str, nargs='+', help='Locations to list')

# w = Writer('name', 'mode', 'nlink', 'size')
w = Writer()
args = parser.parse_args()
if args.locations:
	for location in args.locations:
		ls_location(location, w)
else:
	ls_location('.', w)
w.close()
