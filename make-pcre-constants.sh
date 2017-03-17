#!/usr/bin/env bash

set -eu

awk '/^#define PCRE/ && $3 {print "E("$2");"}' /usr/include/pcre.h | grep -v 'PCRE_UCHAR\|PCRE_SPTR' | sort | xargs -n5
