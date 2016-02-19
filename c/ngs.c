// apt-get install uthash-dev libgc-dev libffi6
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7
// libgc-dev - 1:7.2d-6.4
// libffi6 - 3.2.1-4
#include <assert.h>
#include <errno.h>
#include <unistd.h>
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

char *find_bootstrap_file() {
	static char *places[] = {"/etc/ngs/bootstrap.ngs", "/var/lib/ngs/bootstrap.ngs", "/usr/share/ngs/bootstrap.ngs", NULL};
	char *fname;
	char *home_dir;
	int len;
	char fmt[] = "%s/.bootstrap.ngs";

	fname = getenv("NGS_BOOTSTRAP");
	if(fname) {
		return fname;
	}

	home_dir = getenv("HOME");
	if(home_dir) {
		len = snprintf(NULL, 0, fmt, home_dir) + 1;
		fname = NGS_MALLOC(len);
		snprintf(fname, len, fmt, home_dir);
		// printf("HOME fname: %s\n", fname);
		if(access(fname, F_OK) != -1) {
			return fname;
		}
	}

	for(len=0; places[len]; len++) {
		if(access(places[len], F_OK) != -1) {
			return places[len];
		}
	}

	return NULL;
}

int main(int argc, char **argv)
{
	ast_node *tree = NULL;
	VM vm;
	CTX ctx;
	char *bytecode;
	size_t len;
	VALUE result;
	int parse_ok;
	char *bootstrap_file_name;

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
	bootstrap_file_name = find_bootstrap_file();
	if(!bootstrap_file_name) {
		fprintf(stderr, "Cold not find bootstrap file\n");
		exit(100);
	}
	if(!strcmp(bootstrap_file_name, "-")) {
		yyctx.input_file = stdin;
	} else {
		yyctx.input_file = fopen(bootstrap_file_name, "r");
	}
	if(!yyctx.input_file) {
		fprintf(stderr, "Error while opening bootstrap file '%s': %d - %s\n", bootstrap_file_name, errno, strerror(errno));
		exit(101);
	}
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
	// BROKEN SINCE BYTECODE FORMAT CHANGE // IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	vm_init(&vm, argc, argv);
	vm_load_bytecode(&vm, bytecode);
	ctx_init(&ctx);
	vm_run(&vm, &ctx, 0, &result);
	return 0;
}
