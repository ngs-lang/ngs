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

ngs: *.c *.h
	gcc -g -o $@ ngs.c -lfl

clean:
	-rm parser.h parser.c
	-rm scanner.c
	-rm ngs
