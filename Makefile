default:
	@echo "No target specified"
	exit 1

CMAKE := $(shell command -v cmake3 2> /dev/null)
ifndef CMAKE
	CMAKE := cmake
endif

.PHONY: build
build:
ifeq ($(shell uname -s),Darwin)
	( source build.macos.env.sh && mkdir -p build && cd build && $(CMAKE) .. && make )
else
	( mkdir -p build && cd build && $(CMAKE) .. && make )
endif

.PHONY: tests
tests:
	(cd build && ctest)

.PHONY: install
install: build
	(cd build && sudo make install)

.PHONY: clean
clean:
	rm -rf build

.PHONY: update-vim-syntax
update-vim-syntax:
	./helper-scripts/update-vim-syntax.ngs vim/syntax/ngs.vim

.PHONY: setup-dev-env
setup-dev-env: dir := $(abspath $(shell pwd))
setup-dev-env:
ifeq ($(shell uname -s),Darwin)
	(cd /usr/local/bin; ln -sf "$(dir)/build/ngs")
	(cd /usr/local/lib; ln -sf "$(dir)/lib" ngs)
else
	@echo setup-dev-env is not implemented for your platform
endif
