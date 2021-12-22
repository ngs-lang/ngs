default:
	@echo "No target specified"
	exit 1

.PHONY: build
build:
ifeq ($(shell uname -s),Darwin)
	( source build.macos.env.sh && mkdir -p build && cd build && cmake .. && make )
else
	( mkdir -p build && cd build && cmake .. && make )
endif

.PHONY: tests
tests:
	(cd build && ctest)

.PHONY: install
install: build
	(cd build && sudo make install)

.PHONY: test-installation
test-installation:
	@NGS_PATH=$$(command -v ngs) && echo "-> NGS is available at: $${NGS_PATH}"
	@MAN_PATH=$$(man -w ngs) && echo "-> NGS man is available at: $${MAN_PATH}"

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
