default:
	exit 1
test-lisp:
	time sbcl --script test.lisp
test-js:
	mocha
readme.html: readme.md
	markdown_py readme.md >readme.html
