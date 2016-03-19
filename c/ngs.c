// apt-get install uthash-dev libgc-dev libffi6 libjson-c2
// uthash - http://stackoverflow.com/questions/18197825/looking-for-hash-table-c-library. using version 1.9.7, 1.9.9.1+git20151125-1
// libgc-dev - 1:7.2d-6.4, 1:7.4.2-7.3
// libffi6 - 3.2.1-4
// libjson-c2, libjson-c-dev - 0.11-4
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
	char *ret = NGS_MALLOC_ATOMIC(1024);
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
	IP ip;
	VALUE closure, result;
	int parse_ok;
	char *bootstrap_file_name;
	char *source_file_name;
	METHOD_RESULT mr;

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
		source_file_name = ngs_strdup("<stdin>");
		yyctx.input_file = stdin;
	} else {
		source_file_name = bootstrap_file_name;
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

	// TODO: use native_... methods to load and run the code
	bytecode = compile(tree, source_file_name, &len);
	// BROKEN SINCE BYTECODE FORMAT CHANGE // IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	vm_init(&vm, argc, argv);
	set_global(&vm, "BOOTSTRAP_FILE", make_string(bootstrap_file_name));
	ctx_init(&ctx);
	ip = vm_load_bytecode(&vm, bytecode);
	closure = make_closure_obj(ip, 0, 0, 0, 0, 0, NULL);
	mr = vm_call(&vm, &ctx, &result, closure, 0, NULL);
	if(mr == METHOD_OK) {
		return 0;
	}
	if(mr == METHOD_EXCEPTION) {
#define H(result, obj, key) { HASH_OBJECT_ENTRY *e; assert(IS_HASH(obj)); e = get_hash_key(obj, make_string(key)); assert(e); result = e->val; }
		if(obj_is_of_type(result, vm.Exception)) {
			// TODO: fprintf to stderr and teach dump_titled to optionally fprintf to stderr too
			printf("====== Uncaught exception of type '%s' ======\n", obj_to_cstring(NGS_TYPE_NAME(NORMAL_TYPE_INSTANCE_TYPE(result))));
			// TODO: maybe macro to iterate attributes
			VALUE fields = NGS_TYPE_FIELDS(NORMAL_TYPE_INSTANCE_TYPE(result));
			HASH_OBJECT_ENTRY *e;
			for(e=HASH_HEAD(fields); e; e=e->insertion_order_next) {
				if(obj_is_of_type(ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(result))[GET_INT(e->val)], vm.Backtrace)) {
					// Backtrace.frames = [{"closure": ..., "ip": ...}, ...]
					VALUE backtrace = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(result))[GET_INT(e->val)];
					VALUE frames;
					assert(get_normal_type_instace_attribute(backtrace, make_string("frames"), &frames) == METHOD_OK);
					unsigned int i;
					for(i = 0; i < OBJ_LEN(frames); i++) {
						VALUE frame, resolved_ip, ip;
						frame = ARRAY_ITEMS(frames)[i];
						printf("=== Frame #%u ===\n", i);
						H(ip, frame, "ip");
						resolved_ip = resolve_ip(&vm, (IP)(GET_INT(ip) - 1));
						if(IS_HASH(resolved_ip)) {
							VALUE file, first_line, first_column, last_line, last_column;
							H(file, resolved_ip, "file");
							H(first_line, resolved_ip, "first_line");
							H(first_column, resolved_ip, "first_column");
							H(last_line, resolved_ip, "last_line");
							H(last_column, resolved_ip, "last_column");
							// TODO: fix types
							printf("%s : %d:%d - %d:%d\n", obj_to_cstring(file), (int) GET_INT(first_line), (int) GET_INT(first_column), (int) GET_INT(last_line), (int) GET_INT(last_column));
						} else {
							printf("(no source location)\n");
						}
					}
					continue;
				}
				if(IS_STRING(e->key)) {
					dump_titled(obj_to_cstring(e->key), ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(result))[GET_INT(e->val)]);
				} else {
					// Should not happen
					dump_titled("attribute key", e->key);
					dump_titled("attribute value", ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(result))[GET_INT(e->val)]);
				}
			}

		} else {
			dump_titled("Uncaught exception", result);
		}
		return 1;
#undef H
	}
	assert(0 == "Unexpected exit from bootstrap code");
}
