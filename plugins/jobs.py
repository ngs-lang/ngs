#!/usr/bin/env python3

# ZE_HANDLE: {"path": "/jobs", "methods": ["GET", "POST"]}
# ZE_HANDLE: {"path": "/jobs/$id", "methods": ["GET", "POST"]}

# import argparse
import json
import sys

if sys.argv[1] == 'meta':
    json.dump({
        'desc': 'Run shell jobs',
        'ver': '0.1'
    }, sys.stdout)
    sys.exit(0)
