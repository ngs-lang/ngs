#!/bin/bash

sudo apt-get -y install libgc-dev libffi6 libffi-dev libjson-c-dev peg libpcre3-dev make cmake pandoc pkg-config build-essential
sudo type awk || sudo apt-get install gawk

mkdir build && cd build && cmake .. && make && sudo make install
