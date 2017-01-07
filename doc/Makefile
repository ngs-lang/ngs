all: html man methods.html

methods.html: ../c/vm.c ../c/bootstrap.ngs ../c/stdlib.ngs ../c/autoload/* make_methods.ngs
	./make_methods.ngs >$@

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
	-rm methods.html
