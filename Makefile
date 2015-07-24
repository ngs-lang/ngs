default:
	exit 1
test-lisp:
	time sbcl --script test.lisp
test-js:
	mocha
readme.html: readme.md
	markdown_py readme.md >readme.html
ngs: ngs.lisp run.lisp
	NGS_COMPILE=1 time sbcl --script run.lisp
timing: ngs
	time ./ngs stdlib2.ngs >/dev/null
	time sbcl --script run.lisp stdlib2.ngs >/dev/null
profiling: ngs
	NGS_PROFILE=1 time ./ngs stdlib2.ngs
	NGS_PROFILE=1 time sbcl --script run.lisp stdlib2.ngs
trace:
	NGS_TRACE=1 time sbcl --script run.lisp stdlib2.ngs >trace.txt 2>&1
