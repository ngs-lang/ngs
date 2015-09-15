// apt-get install flex bison uthash-dev
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7
#include "obj.h"
#include "ast.h"
#include "vm.c"
#include "ast.c"
#include "parser.c"
#include "scanner.c"
#include "compile.c"

int yyerror (yyscan_t scanner, char const *s) {
	fprintf (stderr, "%s\n", s);
}

int main (int argc, char * argv[])
{
	ast_node *tree;
	VM vm;
	CTX ctx;

	int ret = 0;
	yyscan_t scanner;
	yylex_init(&scanner);

	ret = yyparse(scanner, &tree);
	yylex_destroy(scanner);

	// printf("Tree: %p\n", tree);
	// printf("Result: %d\n", tree->val.num);
	print_ast(tree, 0);

	// print - start
	char *buf;
	IP *len;
	int i;
	buf = compile(tree, len);
	for(i=0; i<*len; i++) {
		printf("[%d] %d\n", i, buf[i]);
	}
	// print - end
	vm_init(&vm);
	vm_load_bytecode(&vm, buf, *len);
	ctx_init(&ctx);
	vm_run(&vm, &ctx);

	return ret;
}
