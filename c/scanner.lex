%{
#include "parser.h"
%}

%option bison-bridge
%option bison-locations
%option reentrant
%option yylineno

/* TODO: keep track of file name, see http://archive.oreilly.com/pub/a/linux/excerpts/9780596155971/error-reporting-recovery.html */
%{
#define YY_USER_ACTION \
	yylloc->first_line = yylloc->last_line = yylineno; \
	yylloc->first_column = yycolumn; \
	yylloc->last_column = yycolumn + yyleng - 1; \
	yycolumn += yyleng;
%}

blanks          [ \t\n]+
identifier		[_a-zA-Z0-9]+
digits			[0-9]+

%%

{blanks}        { /* ignore */ }

{digits}		{
	yylval->n = atoi(yytext);
	printf("LEX NUMBER: %d\n", yylval->n);
	return NUMBER;
}
