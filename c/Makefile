# TODO: maybe do it with usual .o intermediate files

SHELL := /bin/bash -e

PKG_NAME := ngs
SHARE_FILES := bootstrap.ngs stdlib.ngs cli.ngs autoload
CC := gcc
# -gdwarf-4 -g3 -- for debugging macros
# -rdynamic -- for backtrace_symbols() to work
CFLAGS := -Wall -Wextra -gdwarf-4 -g3 -g -O1
CFLAGS_DEBUG=-DNGS_DEBUG_FLAGS=0xFFFF
LDLIBS := -lm -lpthread -lgc -lffi -ldl -ljson-c -lpcre
LDFLAGS := -rdynamic
# -pg - for gprof profiler
LDFLAGS_PROFILER := -pg -g
COMMON_DEPS := *.h Makefile syntax.c syntax.auto.h pcre_constants.c
SRC := debug.o ast.o obj.o compile.o decompile.o vm.o ngs.o
PCRE_H := /usr/include/pcre.h
# SRC := *.c

all: ngs ngs-prof ngs-debug

# Built for peg/leg v 0.1.15
syntax.c: syntax.leg patch-leg-input.sed patch-leg-output.sed patch-leg-output.awk
	cat $< | sed -f patch-leg-input.sed | leg  | sed -f patch-leg-output.sed | awk -f patch-leg-output.awk >$@

syntax.auto.h: syntax.c Makefile
	cat $< | (sed -n '/^typedef/p; /^struct/,/^$$/p; /define YY_MAX_LINES/p;'; echo 'int yyparse(yycontext *yyctx);'; echo 'yycontext * yyrelease(yycontext *yyctx);') >$@

pcre_constants.c: $(PCRE_H)
	awk '/^#define PCRE/ && $$3 {print "E("$$2");"}' $(PCRE_H) | grep -v 'PCRE_UCHAR\|PCRE_SPTR' | sort | xargs -n5 >$@

%.o: %.c $(COMMON_DEPS)
	$(CC) -c $(CFLAGS) $< -o $@

%_debug.o: %.c $(COMMON_DEPS)
	$(CC) -c $(CFLAGS) $(CFLAGS_DEBUG) $< -o $@

ngs: $(SRC)
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS) $(LDFLAGS)

ngs-prof: $(SRC)
	$(CC) $(CFLAGS) -o $@ $^ $(LDLIBS) $(LDFLAGS) $(LDFLAGS_PROFILER)

ngs-debug: $(patsubst %.o, %_debug.o, $(SRC))
	$(CC) $(CFLAGS) $(CFLAGS_DEBUG) -o $@ $^ $(LDLIBS) $(LDFLAGS)

# Micro bechmark but it's still something
time-sum: ngs

	# About 2.5 seconds on my laptop with gcc -O1
	# About 1.7 seconds on my laptop with clang -O1
	NGS_BOOTSTRAP=bootstrap.ngs time ./ngs -e 's=0;for(i=0;i<10000000;i=i+1) {s=s+i}'

	# About 1.1 seconds / perl 5.20.2
	time perl -e '$$s=0; for($$i=0; $$i<10000000; $$i++) { $$s+=$$i}'

	# About 7 seconds, 10x less work / bash 4.3.30
	time bash -c 's=0; for ((i=0;i<1000000;i=i+1));do s=$$((s+i));done; '

	# About 0.7 seconds / ruby 2.1.5p273 (1.9, 2.0, 2.1 have similar performance)
	time ruby -e 's=0; for i in 0...10000000 do s=s+i end'

	# About 1 second / python 2.7.9
	time python2.7 -c $$'s=0\nfor i in range(0,10000000): s=s+i'

	# About 1.3 seconds / 3.4.2
	time python3.4 -c $$'s=0\nfor i in range(0,10000000): s=s+i'

	# 3.9 seconds, 100x less work / Perl 6 2014.07 built on parrot 6.6.0 revision 0
	time perl6 -e 'my $$s=0; loop (my $$i=0; $$i < 100000; $$i++) {$$s+=$$i}'

	# About 5.5 seconds / TCL 8.6
	time echo 'set s 0; for {set i 0} {$$i < 10000000} {incr i} {set s [expr {$$s + $$i}]}' | tclsh8.6

clean:
	-rm syntax.c pcre_constants.c
	-rm syntax.auto.h
	-rm ngs ngs-prof ngs-debug
	-rm *.o

tags: *.c *.h
	ctags $^

# https://www.gnu.org/prep/standards/html_node/Directory-Variables.html
prefix = /usr/local
datarootdir = $(prefix)/share
datadir = $(datarootdir)
exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin

.PHONY: install
install: ngs
	# TODO: use "install" instead of "cp"
	mkdir -p $(datadir)/$(PKG_NAME)
	cp ngs $(bindir)/
	cp -a $(SHARE_FILES) $(datadir)/$(PKG_NAME)/

.PHONY: uninstall
uninstall:
	rm -rf $(datadir)/$(PKG_NAME)/
	rm $(bindir)/ngs

test-no-bootstrap-no-stdlib: ngs
	@ echo "[ Starting tests ]"
	@ cd tests/ && \
	for f in *.in;do \
		echo "+ Bare test: $${f%.in}"; \
		out=$$(NGS_BOOTSTRAP=- ../ngs <$$f) && \
		diff -u <(echo "$$out") $${f%.in}.ok || exit 1; \
	done;

test-stdlib: ngs
	@ ./test.ngs

test: test-no-bootstrap-no-stdlib test-stdlib
	@ echo "ALL TESTS OK"

test-on-jessie:
	make clean
	make CC=gcc-4.8
	NGS_DIR=. make test
	make clean
	make CC=gcc-4.9
	NGS_DIR=. make test
	make clean
	make CC=clang-3.5
	NGS_DIR=. make test

test-on-stretch:
	make clean
	make CC=gcc-4.8
	NGS_DIR=. make test
	make clean
	make CC=gcc-4.9
	NGS_DIR=. make test
	make clean
	make CC=gcc-5
	NGS_DIR=. make test
	make clean
	make CC=clang-3.6
	NGS_DIR=. make test

test-on-trusty:
	make clean
	make CC=gcc-4.8
	NGS_DIR=. make test
	make clean
	make CC=clang
	NGS_DIR=. make test

