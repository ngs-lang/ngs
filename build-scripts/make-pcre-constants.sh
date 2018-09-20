#!/usr/bin/env bash

set -eu
set -o pipefail

H="$1"

awk '/^#define PCRE/ && $3 {print "E("$2");"}' $H | grep -v 'PCRE_UCHAR\|PCRE_SPTR' | sort | xargs -n5
