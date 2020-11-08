#!/bin/bash

set -e

sudo apt-get install -y libgc-dev libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
type awk || sudo apt-get install -y gawk

make clean install