%{
/* Eliminate warnings - start */
#define YY_NO_INPUT
/* Eliminate warnings - end */
#include "parser.h"
%}

/* TODO: multibyte characters support? */

%option bison-bridge
%option bison-locations
%option reentrant
%option yylineno
%option stack

/* Eliminate warnings - start */
%option nounput
%option noyy_top_state
/* Eliminate warnings - end */

/* TODO: keep track of file name, see http://archive.oreilly.com/pub/a/linux/excerpts/9780596155971/error-reporting-recovery.html */
%{
#define YY_USER_ACTION \
	yylloc->first_line = yylloc->last_line = yylineno; \
	yylloc->first_column = yycolumn; \
	yylloc->last_column = yycolumn + yyleng - 1; \
	yycolumn += yyleng;

#define USE_TEXT_AS_NAME yylval->name = strdup(yytext) /* strdup needed?*/
%}


/* INITIAL is commands */
%x CODE

blanks          [ \t\n]+
identifier		[_a-zA-Z]+[_a-zA-Z0-9]*
digits			[0-9]+

%%

{blanks}            { /* ignore */ }

<INITIAL>"{"        { yy_push_state(CODE, yyscanner); DEBUG_PARSER("%s", "Entering mode: CODE\n"); return '{'; }

<CODE>{
	"+"|"-"|"<"|">" { DEBUG_PARSER("%s", "LEX BINOP\n"); USE_TEXT_AS_NAME; return BINOP; }
	"="             { DEBUG_PARSER("%s", "LEX EQUALS\n"); return '='; }
	{digits}		{ yylval->number = atoi(yytext); DEBUG_PARSER("LEX NUMBER %d\n", yylval->number); return NUMBER; }
	"while"         { DEBUG_PARSER("%s", "LEX WHILE\n"); return WHILE; }
	"for"           { DEBUG_PARSER("%s", "LEX FOR\n"); return FOR; }
	{identifier}    { DEBUG_PARSER("LEX IDENTIFIER %s\n", yytext); USE_TEXT_AS_NAME; return IDENTIFIER; }
	";"             { DEBUG_PARSER("LEX E.DELIMITER %s\n", yytext); return *yytext; }
	"("|")"         { DEBUG_PARSER("LEX PAREN %s\n", yytext); return *yytext; }
	"{"             { yy_push_state(CODE, yyscanner); DEBUG_PARSER("%s", "Re-entering mode: CODE\n"); return '{'; }
	"}"             { yy_pop_state(yyscanner); DEBUG_PARSER("%s", "Leaving mode: CODE\n"); return '}'; }
}
