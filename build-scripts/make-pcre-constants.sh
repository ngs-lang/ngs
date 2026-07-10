#!/usr/bin/env bash

set -eu
set -o pipefail

H="$1"

awk '
	/^#define[ \t]+PCRE2_[A-Z0-9_]+[ \t]+\(?-?(0x[0-9A-Fa-f]+|[0-9])/ {
		name = $2
		if (name ~ /^PCRE2_(DATE|PRERELEASE|LOCAL_WIDTH)/) next
		printf "E(%s);\n", name
	}
' "$H"
