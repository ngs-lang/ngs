#!/usr/bin/env bash

# Migrating from install-mac.sh and install-linux.sh to this file.
# When tested, the files above can be removed.

set -e

OS=`uname`

echo "+ Installing dependencies"

if [[ "$OS" == "Darwin" ]]; then
	echo "  + On Mac"
	brew install libgc libffi peg cmake pandoc awk make pkg-config json-c pcre gnu-sed
else
	echo "  + On Linux"
	sudo apt-get install -y libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
	type awk || sudo apt-get install -y gawk
fi

echo "+ Installing NGS"
make clean install
