all: ngsmet.1.md ngstyp.1.md html man

ngsmet.1.md: ../c/vm.c ../c/bootstrap.ngs ../c/stdlib.ngs ../c/autoload/* ngsmet.header make_methods_markdown.ngs
	(cat ngsmet.header ; ./make_methods_markdown.ngs methods) >$@

ngstyp.1.md: ../c/vm.c ../c/bootstrap.ngs ../c/stdlib.ngs ../c/autoload/* ngstyp.header make_methods_markdown.ngs
	(cat ngstyp.header ; ./make_methods_markdown.ngs types) >$@

html: $(patsubst %.1.md,%.1.html,$(wildcard *.1.md)) ngsmet.1.md ngstyp.1.md
man: $(patsubst %.1.md,%.1,$(wildcard *.1.md))

%.1.html: %.1.md pandoc.css
	pandoc -f markdown+hard_line_breaks -t html -c pandoc.css -s $< -o $@
%.1: %.1.md
	pandoc -f markdown+hard_line_breaks -t man -s $< -o $@

.PHONY: clean
clean:
	-rm *.1
	-rm *.1.html
	-rm ngsmet.1.md
	-rm ngstyp.1.md
