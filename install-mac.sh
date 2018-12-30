#!/bin/bash

set -e

brew install libgc libffi peg cmake pandoc awk make

make install
