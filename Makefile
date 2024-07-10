default:
	@echo "No target specified"
	exit 1

CMAKE := $(shell command -v cmake3 2> /dev/null)
ifndef CMAKE
	CMAKE := cmake
endif

CTEST := $(shell command -v ctest3 2> /dev/null)
ifndef CTEST
	CTEST := ctest
endif

SUDO := $(shell command -v sudo 2> /dev/null)

.PHONY: build
build:
	( mkdir -p build && cd build && $(CMAKE) .. && make )

.PHONY: tests
tests:
	(cd build && $(CTEST))

.PHONY: install
install: build
	(cd build && $(SUDO) make install)

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
	(cd /usr/local/bin && ln -sf "$(dir)/build/ngs")
	(cd /usr/local/lib && ln -sf "$(dir)/lib" ngs)
else
	@echo setup-dev-env is not implemented for your platform
endif
