#!/bin/bash

set -eu

F=VERSION
SRC_DIR="$1"

commit_timestamp=$(git log -1 --format='%ct')
v=$(date -u +'%Y%m%d-%H%M%S' -d "@$commit_timestamp")

dirty=""

if git status --porcelain 2>/dev/null | grep -v debian/ | awk '{print $1}' | grep -q '^M';then
	dirty="-dirty"
fi

ver="$(<"$SRC_DIR/version-prefix").$v$dirty"
old_ver=$(cat $F 2>/dev/null || true)

if [[ "$old_ver" != "$ver" ]];then
	echo "$ver" >"$F"
fi
