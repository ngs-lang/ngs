%{
#include <stdio.h>
%}

%define api.pure full
/* cheat a little: supposed to be scanner_t scanner */
%parse-param { void * scanner }
%lex-param   { void * scanner }
%locations

// Symbols.
%union
{
	int		n;
	ast_node node;
};
%token <sval> IDENTIFIER
%token <n> NUMBER
%token PROCEDURE
%token BLOCK_START
%token BLOCK_END

%start Program
%%

Program:
	/* empty */
	Numbers
	;

Numbers:
	/* empty */
	| Numbers Number
	;

Number:
	NUMBER  { printf("\t\tNumber : %d at line %d\n", $1, @1.first_line); }
	;
%%

