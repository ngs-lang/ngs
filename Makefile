default:
	@echo "No target defined"

.PHONY: build
build:
ifeq ($(shell uname -s),Darwin)
	( source build.macos.env.sh && mkdir -p build && cd build && cmake .. && make )
else
	( mkdir -p build && cd build && cmake .. && make )
endif

.PHONY: install
install: build
	(cd build && sudo make install)

.PHONY: clean
clean:
	rm -rf build
