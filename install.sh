#!/usr/bin/env bash

set -e

OS=`uname`

if type sudo &>/dev/null;then
	SUDO=sudo
else
	SUDO=""
fi

echo "+ Installing dependencies"

if [[ "$OS" == "Darwin" ]]; then
	echo "  + On Mac"
	brew install libgc libffi peg cmake pandoc awk make pkg-config json-c pcre gnu-sed
else
	echo "  + On Linux"
	$SUDO apt-get install -y libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
	type awk || $SUDO apt-get install -y gawk
fi

echo "+ Installing NGS"
make clean install
