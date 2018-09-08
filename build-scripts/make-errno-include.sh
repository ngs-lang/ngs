#!/bin/bash

set -eu
set -o pipefail

FILE="$1"

cpp -E -dD "$FILE" | awk '
	/#define E/ { print "E(" $2 ");" }
'
