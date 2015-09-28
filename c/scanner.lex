%{
#include "parser.h"
%}

/* TODO: multibyte characters support? */

%option bison-bridge
%option bison-locations
%option reentrant
%option yylineno
%option stack

/* TODO: keep track of file name, see http://archive.oreilly.com/pub/a/linux/excerpts/9780596155971/error-reporting-recovery.html */
%{
#define YY_USER_ACTION \
	yylloc->first_line = yylloc->last_line = yylineno; \
	yylloc->first_column = yycolumn; \
	yylloc->last_column = yycolumn + yyleng - 1; \
	yycolumn += yyleng;

%}

/* INITIAL is commands */
%x CODE

blanks          [ \t\n]+
identifier		[_a-zA-Z0-9]+
digits			[0-9]+

%%

{blanks}            { /* ignore */ }

<INITIAL>"{"        { yy_push_state(CODE, yyscanner); DEBUG_PARSER("%s", "Entering mode: CODE\n"); }
<CODE>"}"           { yy_pop_state(yyscanner); DEBUG_PARSER("%s", "Leaving mode: CODE\n"); }

<CODE>{
	"+"|"-"         { DEBUG_PARSER("%s", "LEX BINOP +\n"); yylval->name = strdup(yytext) /* strdup needed?*/; return BINOP; }
	{digits}		{ yylval->number = atoi(yytext); DEBUG_PARSER("LEX NUMBER %d\n", yylval->number); return NUMBER; }
}
