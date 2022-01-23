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
		$SUDO yum install -y gc-devel libffi-devel json-c-devel pcre-devel make cmake3 pandoc pkgconfig
		if ! command -v ctest &>/dev/null;then
			if command -v ctest3 &>/dev/null;then
				echo "    + Ctest3 found, configuring as default"
				alternatives --install /usr/local/bin/cmake cmake /usr/bin/cmake3 20 --slave /usr/local/bin/ctest ctest /usr/bin/ctest3 --slave /usr/local/bin/cpack cpack /usr/bin/cpack3 --slave /usr/local/bin/ccmake ccmake /usr/bin/ccmake3 --family cmake
			else
				echo "    + Installing ctest"
				$SUDO yum install -y ctest
		  fi
		fi
		$SUDO yum groupinstall -y "Development Tools"
	elif type pacman &>/dev/null;then
		echo "  + On Linux / pacman. Installing apt packages."
		$SUDO pacman -Sy --noconfirm peg make cmake pandoc pkgconfig
	elif type apt &>/dev/null;then
		echo "  + On Linux / apt. Installing apt packages."
		$SUDO apt-get install -y libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
		type awk || $SUDO apt-get install -y gawk
	else
		echo "ERROR: No supported package manager found! Accepted package managers are: apt, yum and pacman. Open an issue in github if any other is required."
		exit 1
	fi
fi

echo "+ Installing NGS"
make clean install
