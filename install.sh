#!/usr/bin/env bash

set -eu

OS=`uname`

if type sudo &>/dev/null;then
	SUDO=sudo
else
	SUDO=""
fi

echo "+ Installing dependencies"

if [[ "$OS" == "Darwin" ]]; then
	echo "  + On Mac, installing brew packages"
	brew install libgc libffi peg cmake pandoc awk make pkg-config json-c pcre gnu-sed
else
	# Presumably Linux
	if type yum &>/dev/null;then
		echo "  + On Linux / yum"
		if type amazon-linux-extras &>/dev/null;then
			echo "    + Enabling EPEL (provides pandoc)"
			$SUDO amazon-linux-extras install epel -y
		fi
		# * peg/leg is compiled by CMake from sources and used during build without installing
		#   that is because I did not find it packaged.
		echo "    + Installing yum packages"
		$SUDO yum install -y gc-devel libffi-devel json-c-devel pcre-devel make cmake3 ctest pandoc pkgconfig
		$SUDO yum groupinstall -y "Development Tools"
	else
		echo "  + On Linux / apt. Installing apt packages."
		$SUDO apt-get install -y libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
		type awk || $SUDO apt-get install -y gawk
	fi
fi

echo "+ Installing NGS"
make clean install
