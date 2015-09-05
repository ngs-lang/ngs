#include "ast.h"
#include "parser.c"
#include "scanner.c"

int yyerror (yyscan_t scanner, char const *s) {
	fprintf (stderr, "%s\n", s);
}

int main (int argc, char * argv[])
{
	int ret = 0;
	yyscan_t scanner;
	yylex_init(&scanner);

	ret = yyparse(scanner);
	yylex_destroy(scanner);
	return ret;
}
