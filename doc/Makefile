all: man

man: $(patsubst %.1.md,%.1,$(wildcard *.1.md))

%.1: %.1.md
	pandoc -f markdown+hard_line_breaks -t man -s $< -o $@

.PHONY: clean
clean:
	-rm *.1
