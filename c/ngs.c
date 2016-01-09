// apt-get install flex bison uthash-dev libgc-dev
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7
// libgc-dev - 1:7.2d-6.4
#include <assert.h>
#include "ngs.h"
#include "syntax.c"
#undef __
#include "compile.h"
#include "decompile.h"
#include "vm.h"


int main()
{
	ast_node *tree = NULL;
	VM vm;
	CTX ctx;
	char *bytecode;
	size_t len;
	VALUE result;
	int parse_ok;

	NGS_GC_INIT();
	// (causes warning) // NGS_GC_THR_INIT();

	yycontext yyctx;
	memset(&yyctx, 0, sizeof(yycontext));
	parse_ok = yyparse(&yyctx);
	printf("DONE\n");
	printf("parse_ok %d\n", parse_ok);

	tree = yyctx.__;
	printf("tree %p\n", tree);
	IF_DEBUG(COMPILER, print_ast(tree, 0);)

	yyrelease(&yyctx);

	exit(1);

	bytecode = compile(tree, &len);
	IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	vm_init(&vm);
	vm_load_bytecode(&vm, bytecode, len);
	ctx_init(&ctx);
	vm_run(&vm, &ctx, 0, &result);
}
