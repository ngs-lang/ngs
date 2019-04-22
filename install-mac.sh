#!/bin/bash

set -e

brew install libgc libffi peg cmake pandoc awk make pkg-config json-c

make clean install
