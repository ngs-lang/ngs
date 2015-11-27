// apt-get install flex bison uthash-dev libgc-dev
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7
// libgc-dev - 1:7.2d-6.4
#include <assert.h>
#include "parser.c"
#include "scanner.c"
#include "ngs.h"
#include "compile.h"
#include "decompile.h"
#include "vm.h"


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
	char *bytecode;
	size_t len;
	VALUE result;

	NGS_GC_INIT();
	// (causes warning) // NGS_GC_THR_INIT();

	int ret = 0;
	yyscan_t scanner;
	yylex_init(&scanner);

	ret = yyparse(scanner, &tree);
	assert(ret == 0);
	yylex_destroy(scanner);

	IF_DEBUG(COMPILER, print_ast(tree, 0);)

	bytecode = compile(tree, &len);
	IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	vm_init(&vm);
	vm_load_bytecode(&vm, bytecode, len);
	ctx_init(&ctx);
	vm_run(&vm, &ctx, 0, &result);

	return ret;
}
