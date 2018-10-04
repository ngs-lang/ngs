#!/bin/bash

brew bundle --file=- <<-EOS
brew "libgc"
brew "libffi"
brew "peg"
brew "cmake"
brew "pandoc"
brew "awk"
brew "make"
EOS

source build.macos.env.sh
make build
