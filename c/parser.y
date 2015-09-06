%{
#include <stdio.h>
#define NODE(name) ast_node *name = malloc(sizeof(ast_node))
%}

%define api.pure full
/* cheat a little: supposed to be scanner_t scanner */
%parse-param { void * scanner }
%parse-param { ast_node ** result }
%lex-param   { void * scanner }
%locations

// Symbols.
%union
{
	int		n;
	/*ast_node node;*/
};
%token <n> NUMBER
%type <n> Number
%type <n> Numbers
/*%type <ast_node> Program*/

%start Program
%%

Program:
	Numbers { printf("P0\n"); NODE(ret); ret->val.num = $1; *result=ret; }
	;

Numbers:
	/* empty */ { printf("numbers-empty\n"); $$ = 0;}
	| Numbers Number { printf("numbers-something %d %d\n", $1, $2); $$=$1+$2; }
	;

Number:
	NUMBER  {
		printf("+ Number : %d at line %d\n", $1, @1.first_line);
		// NODE(ret);
		// ret->val.num = yylval.n;
		// result = malloc(sizeof(ast_node));
		// return yylval.n;
	}
	;
%%

