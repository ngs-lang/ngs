#include "obj.h"
#include "ast.h"
#include "ast.c"
#include "parser.c"
#include "scanner.c"

int yyerror (yyscan_t scanner, char const *s) {
	fprintf (stderr, "%s\n", s);
}

int main (int argc, char * argv[])
{
	ast_node *tree;
	int ret = 0;
	yyscan_t scanner;
	yylex_init(&scanner);

	ret = yyparse(scanner, &tree);
	yylex_destroy(scanner);

	// printf("Tree: %p\n", tree);
	// printf("Result: %d\n", tree->val.num);
	print_ast(tree, 0);
	return ret;
}
