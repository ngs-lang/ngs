#!/bin/bash

set -eu
set -o pipefail

FILE="$1"

${CC:-cpp} -E -dD "$FILE" | awk '
	/#define E/ { print "E(" $2 ");" }
'
