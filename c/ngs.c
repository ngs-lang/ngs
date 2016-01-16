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

char *sprintf_position(yycontext *yy, int pos) {
	int linecol[2];
	char *ret = NGS_MALLOC(1024);
	position_to_line_col(yy, pos, linecol);
	snprintf(ret, 1024, "%d:%d", linecol[0], linecol[1]);
	return ret;
}

int main()
{
	ast_node *tree = NULL;
	VM vm;
	CTX ctx;
	char *bytecode;
	size_t len;
	VALUE result;
	int parse_ok;

	// Silence GCC -Wunused-function
	if(0) { yymatchDot(NULL); yyAccept(NULL, 0); }

	NGS_GC_INIT();
	// (causes warning) // NGS_GC_THR_INIT();

	yycontext yyctx;
	memset(&yyctx, 0, sizeof(yycontext));
	yyctx.fail_pos = -1;
	yyctx.fail_rule = "(unknown)";
	yyctx.lines = 0;
	yyctx.lines_postions[0] = 0;
	parse_ok = yyparse(&yyctx);
	// printf("parse_ok %d\n", parse_ok);
	if(!parse_ok) {
		fprintf(stderr, "NGS: Failed to parse at position %d (%s), rule %s. Exiting.\n", yyctx.fail_pos, sprintf_position(&yyctx, yyctx.fail_pos), yyctx.fail_rule);
		exit(2);
	}

	tree = yyctx.__;
	IF_DEBUG(COMPILER, print_ast(tree, 0);)

	yyrelease(&yyctx);

	bytecode = compile(tree, &len);
	IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	vm_init(&vm);
	vm_load_bytecode(&vm, bytecode, len);
	ctx_init(&ctx);
	vm_run(&vm, &ctx, 0, &result);
	return 0;
}
