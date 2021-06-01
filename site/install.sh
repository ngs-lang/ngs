#!/usr/bin/env bash

# Requires: curl, sudo

set -eu

echo "+ Will install Next Generation Shell"
echo "+ If anything goes wrong, please open an issue at https://github.com/ngs-lang/ngs/issues"

echo "+ Creating temporary directory"

# mktemp template argument is a must for MacOS, do not remove
d=$(mktemp -d ngs.XXXXXXXX)

echo "  + Temporary directory is at $d"

(
	cd "$d"
	echo "+ Downloading and extracting NGS"

	curl -L https://github.com/ngs-lang/ngs/archive/master.tar.gz | tar --strip-components=1 -xzf -

	echo "+ Running installer"
	./install.sh
)

echo "+ Removing temporary directory $d"

rm -rf "$d"

echo "+ NGS installed successfully. Have fun!"
