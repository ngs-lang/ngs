#!/bin/bash

set -e

sudo apt-get install -y uthash-dev libgc-dev libffi6 libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
sudo type awk || sudo apt-get install gawk

make install
