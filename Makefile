all: ngs

.PHONY: test
test:
	time sbcl --script test.lisp

readme.html: readme.md
	markdown_py readme.md >readme.html

ngs: ngs.lisp run.lisp
	NGS_COMPILE=1 time sbcl --script run.lisp

timing: ngs
	NGS_SKIP_STDLIB=1 time ./ngs stdlib.ngs >/dev/null
	NGS_SKIP_STDLIB=1 time sbcl --script run.lisp stdlib.ngs >/dev/null

profiling: ngs
	NGS_PROFILE=1 NGS_SKIP_STDLIB=1 time ./ngs stdlib.ngs
	NGS_PROFILE=1 NGS_SKIP_STDLIB=1 time sbcl --script run.lisp stdlib.ngs

trace-syntax:
	NGS_TRACE_SYNTAX=1 time sbcl --script run.lisp stdlib.ngs >trace.txt 2>&1
