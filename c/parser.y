%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "ngs.h"
#define NODE(name) \
	ast_node *name = calloc(sizeof(ast_node), 1); \
	DEBUG_PARSER("Allocated ast_node at %p\n", name);
#define NODET(name, type_) NODE(name); name->type = type_;
#define SET_LOC(src) \
	yyloc.first_line   = src.first_line; \
	yyloc.first_column = src.first_column; \
	yyloc.last_line    = src.first_column; \
	yyloc.last_column  = src.last_column;
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
	int      number;
	char     *name;
	ast_node *ast_node;
};
%token <number> NUMBER
%token <name> BINOP
%type <ast_node> top_level
%type <ast_node> number

/*TODO: intern symbols*/

%%

top_level: number BINOP number {
		 $1->next_sibling = $3;
		 DEBUG_PARSER("top_level $1 %p %3 %p\n", $1, $3);
		 NODET(ret, BINOP);
		 ret->name = $2;
		 ret->first_child = $1;
		 @$ = @2; // is it ok to do this?
		 // $$ = ret;
		 SET_LOC(@2);
		 *result = ret;
}

number: NUMBER { NODET(ret, NUMBER); ret->number = $1; $$ = ret; }

%%
