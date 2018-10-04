default:
	@echo "No option defined"

.PHONY: build
build:
	( mkdir -p build && cd build && cmake .. && make && sudo make install )
clean:
	rm -rf build

install-mac:
	brew bundle --file << EOF
	set -e  && brew install libgc libffi peg cmake pandoc awk 
	EOF
