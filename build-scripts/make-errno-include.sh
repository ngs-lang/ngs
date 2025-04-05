#!/usr/bin/env bash

set -eu
set -o pipefail

FILE="$1"

${CXX:-cpp} -E -dD "$FILE" | awk '
	/#define E/ { print "E(" $2 ");" }
'
