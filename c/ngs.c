// apt-get install flex bison uthash-dev libgc-dev
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7
// libgc-dev - 1:7.2d-6.4
#include "ngs.h"
#include "obj.h"
#include "obj.c"
#include "ast.h"
#include "vm.c"
#include "ast.c"
#include "parser.c"
#include "scanner.c"
#include "compile.c"
#include "decompile.c"

int yyerror (yyscan_t scanner, char const *s) {
	// Eliminate warning about unused `scanner` variable
	(void)(scanner);
	fprintf (stderr, "PARSE ERROR: <<%s>>\n", s);
	return 0;
}

int main()
{
	ast_node *tree = NULL;
	VM vm;
	CTX ctx;

	NGS_GC_INIT();
	// (causes warning) // NGS_GC_THR_INIT();

	int ret = 0;
	yyscan_t scanner;
	yylex_init(&scanner);

	ret = yyparse(scanner, &tree);
	yylex_destroy(scanner);

	// printf("Tree: %p\n", tree);
	// printf("Result: %d\n", tree->val.num);
	print_ast(tree, 0);
	assert(ret == 0);

	// print - start
	char *buf;
	size_t len;
	buf = compile(tree, &len);
	if(debug_flags & DEBUG_FLAG_COMPILER) {
		decompile(buf, 0, len);
	}
	// print - end
	vm_init(&vm);
	vm_load_bytecode(&vm, buf, len);
	ctx_init(&ctx);
	vm_run(&vm, &ctx);

	return ret;
}
