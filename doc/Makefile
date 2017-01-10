all: html man types.html methods.html

types.html: ../c/vm.c ../c/bootstrap.ngs ../c/stdlib.ngs ../c/autoload/* make_methods.ngs
	./make_methods.ngs types >$@

methods.html: ../c/vm.c ../c/bootstrap.ngs ../c/stdlib.ngs ../c/autoload/* make_methods.ngs
	./make_methods.ngs methods >$@

html: $(patsubst %.1.md,%.1.html,$(wildcard *.1.md))
man: $(patsubst %.1.md,%.1,$(wildcard *.1.md))

%.1.html: %.1.md pandoc.css
	pandoc -f markdown+hard_line_breaks -t html -c pandoc.css -s $< -o $@
%.1: %.1.md
	pandoc -f markdown+hard_line_breaks -t man -s $< -o $@

.PHONY: clean
clean:
	-rm *.1
	-rm *.1.html
	-rm types.html
	-rm methods.html
