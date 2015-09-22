all: ngs

scanner.lex:
	# nothing
parser.y:
	# nothing

parser.h: parser.y
	bison --defines=parser.h -o parser.c parser.y

parser.c: parser.y
	bison --defines=parser.h -o parser.c parser.y

scanner.c: scanner.lex
	flex -o $@ $<

ngs: *.c *.h Makefile
	# -gdwarf-2 -g3 -- for debugging macros
	gcc -gdwarf-2 -g3 -g -o $@ ngs.c -lfl -lpthread -lgc

clean:
	-rm parser.h parser.c
	-rm scanner.c
	-rm ngs
