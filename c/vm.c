#include <assert.h>
#include <dlfcn.h>
#include <inttypes.h>
#include <stdarg.h>

// ..., FMEMOPEN(3)
#include <stdio.h>

// OPEN(2), LSEEK(2), WAIT(2)
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>

// READ(2), LSEEK(2), FORK(2), EXECVE(2), DUP2(2)
#include <unistd.h>

// BCMP(3)
#include <strings.h>

#include <errno.h>

#include "ngs.h"
#include "vm.h"
#include "ast.h"
#include "compile.h"
#include "decompile.h"
#include "syntax.h"
#include "syntax.auto.h"

extern char **environ;

// in ngs.c:
char *sprintf_position(yycontext *yy, int pos);

char BYTECODE_SIGNATURE[] = "NGS BYTECODE";

char *opcodes_names[] = {
	/*  0 */ "HALT",
	/*  1 */ "PUSH_NULL",
	/*  2 */ "PUSH_FALSE",
	/*  3 */ "PUSH_TRUE",
	/*  4 */ "PUSH_INT",
	/*  5 */ "PUSH_REAL",
	/*  6 */ "PUSH_L_STR",
	/*  7 */ "DUP",
	/*  8 */ "POP",
	/*  9 */ "XCHG",
	/* 10 */ "RESOLVE_GLOBAL",
	/* 11 */ "PATCH",
	/* 12 */ "FETCH_GLOBAL",
	/* 13 */ "STORE_GLOBAL",
	/* 14 */ "FETCH_LOCAL",
	/* 15 */ "STORE_LOCAL",
	/* 16 */ "CALL",
	/* 17 */ "CALL_EXC",
	/* 18 */ "CALL_ARR",
	/* 19 */ "RET",
	/* 20 */ "JMP",
	/* 21 */ "JMP_TRUE",
	/* 22 */ "JMP_FALSE",
	/* 23 */ "MAKE_ARR",
	/* 24 */ "MAKE_CLOSURE",
	/* 25 */ "TO_STR",
	/* 26 */ "MAKE_STR",
	/* 27 */ "PUSH_EMPTY_STR",
	/* 28 */ "GLOBAL_DEF_P",
	/* 29 */ "LOCAL_DEF_P",
	/* 30 */ "DEF_GLOBAL_FUNC",
	/* 31 */ "DEF_LOCAL_FUNC",
	/* 32 */ "FETCH_UPVAR",
	/* 33 */ "STORE_UPVAR",
	/* 34 */ "UPVAR_DEF_P",
	/* 35 */ "DEF_UPVAR_FUNC",
	/* 36 */ "MAKE_HASH",
	/* 37 */ "TO_BOOL",
	/* 38 */ "TO_ARR",
	/* 39 */ "TO_HASH",
	/* 40 */ "ARR_APPEND",
	/* 41 */ "ARR_CONCAT",
	/* 42 */ "GUARD",
	/* 43 */ "TRY_START",
	/* 44 */ "TRY_END",
	/* 45 */ "ARR_REVERSE",
	/* 46 */ "THROW",
	/* 47 */ "MAKE_CMD",
	/* 48 */ "SET_CLOSURE_NAME",
	/* 49 */ "SET_CLOSURE_DOC",
	/* 50 */ "HASH_SET",
	/* 51 */ "HASH_UPDATE",
	/* 52 */ "PUSH_KWARGS_MARKER",
	/* 53 */ "MAKE_REDIR",
};


#define EXPECT_STACK_DEPTH(n) assert(ctx->stack_ptr >= (n));
#define PUSH(v) assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr++] = v
#define PUSH_NOCHECK(v) ctx->stack[ctx->stack_ptr++] = v
#define POP(dst) assert(ctx->stack_ptr); ctx->stack_ptr--; dst = ctx->stack[ctx->stack_ptr]
#define POP_NOCHECK(dst) ctx->stack_ptr--; dst = ctx->stack[ctx->stack_ptr]
#define FIRST (ctx->stack[ctx->stack_ptr-1])
#define TOP FIRST
#define SECOND (ctx->stack[ctx->stack_ptr-2])
#define THIRD (ctx->stack[ctx->stack_ptr-3])
#define DUP assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr] = ctx->stack[ctx->stack_ptr-1]; ctx->stack_ptr++;
#define REMOVE_TOP assert(ctx->stack_ptr); ctx->stack_ptr--
#define REMOVE_TOP_NOCHECK ctx->stack_ptr--
#define REMOVE_TOP_N(n) DEBUG_VM_RUN("Popping %d argument(s) after call from stack\n", (int)n); assert(ctx->stack_ptr >= (unsigned int)n); ctx->stack_ptr-=n;
#define REMOVE_TOP_N_NOCHECK(n) DEBUG_VM_RUN("Popping %d argument(s) after call from stack\n", (int)n); ctx->stack_ptr-=n;
#define GLOBALS (vm->globals)
#define THIS_FRAME (ctx->frames[ctx->frame_ptr-1])
#define THIS_FRAME_CLOSURE (THIS_FRAME.closure)
#define LOCALS (THIS_FRAME.locals)
#define UPLEVELS CLOSURE_OBJ_UPLEVELS(THIS_FRAME_CLOSURE)
#define ARG(name, type) name = *(type *) &vm->bytecode[ip]; ip += sizeof(type);
#define ARG_LVI ARG(lvi, LOCAL_VAR_INDEX);
#define ARG_GVI ARG(gvi, GLOBAL_VAR_INDEX);
#define ARG_UVI ARG(uvi, UPVAR_INDEX);
#define PUSH_FUNC(dst, fn) if(IS_NGS_TYPE(dst)) { array_push(NGS_TYPE_CONSTRUCTORS(dst), (fn)); } else { array_push(dst, fn); };
// TODO: Exception
#define CONVERTING_OP(test, type) \
	assert(ctx->stack_ptr); \
	v = TOP; \
	if(test(v)) { \
		goto main_loop; \
	} \
	PUSH(v); \
	THIS_FRAME.last_ip = ip; \
	mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-2], (VALUE){.ptr = vm->type}, 1, &ctx->stack[ctx->stack_ptr-1]); \
	if(mr != METHOD_OK) { \
		dump_titled("Failed to convert to type", (VALUE){.ptr = vm->type}); \
		dump_titled("Failed to convert value", ctx->stack[ctx->stack_ptr-1]); \
		assert(0 == "Failed to convert"); \
	} \
	REMOVE_TOP_N(1); \
	goto main_loop;

#define METHOD_PARAMS (VALUE *argv, VALUE *result)
#define EXT_METHOD_PARAMS (VM *vm, CTX *ctx, VALUE *argv, VALUE *result)
#define METHOD_RETURN(v) { *result = (v); return METHOD_OK; }
#define THROW_EXCEPTION(t) { *result = make_string(t); return METHOD_EXCEPTION; }
#define THROW_EXCEPTION_INSTANCE(e) { \
	set_normal_type_instance_attribute(e, make_string("backtrace"), make_backtrace(vm, ctx)); \
	*result = e; \
	return METHOD_EXCEPTION; \
}

#define INT_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _int_int METHOD_PARAMS { \
	SET_INT(*result, GET_INT(argv[0]) op GET_INT(argv[1])); \
	return METHOD_OK; \
}

#define INT_CMP_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _int_int METHOD_PARAMS { \
	SET_BOOL(*result, GET_INT(argv[0]) op GET_INT(argv[1])); \
	return METHOD_OK; \
}

#define REAL_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _real_real METHOD_PARAMS { \
	METHOD_RETURN(make_real(GET_REAL(argv[0]) op GET_REAL(argv[1]))); \
}

#define REAL_CMP_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _real_real METHOD_PARAMS { \
	SET_BOOL(*result, GET_REAL(argv[0]) op GET_REAL(argv[1])); \
	return METHOD_OK; \
}

#define ARG_LEN(n) OBJ_LEN(argv[n])
#define ARG_DATA_PTR(n) OBJ_DATA_PTR(argv[n])

#define BYTECODE_ADD(ptr, type, val) \
	*(type *) ptr = val; \
	ptr += sizeof(type);

#define BYTECODE_GET(dst, ptr, type) \
	dst = *(type *) ptr; \
	ptr += sizeof(type);

INT_METHOD(plus, +)
INT_METHOD(minus, -)
INT_METHOD(mul, *)
INT_METHOD(div, /)
INT_METHOD(mod, %)
INT_CMP_METHOD(less, <)
INT_CMP_METHOD(less_eq, <=)
INT_CMP_METHOD(greater, >)
INT_CMP_METHOD(greater_eq, >=)
INT_CMP_METHOD(eq, ==)

REAL_METHOD(plus, +)
REAL_METHOD(minus, -)
REAL_METHOD(mul, *)
REAL_METHOD(div, /)
REAL_CMP_METHOD(less, <)
REAL_CMP_METHOD(less_eq, <=)
REAL_CMP_METHOD(greater, >)
REAL_CMP_METHOD(greater_eq, >=)
REAL_CMP_METHOD(eq, ==)

METHOD_RESULT native_dump_any METHOD_PARAMS {
	dump(argv[0]);
	SET_NULL(*result);
	return METHOD_OK;
}

METHOD_RESULT native_echo_str METHOD_PARAMS {
	printf("%s\n", obj_to_cstring(argv[0]));
	METHOD_RETURN(MAKE_NULL);
}

METHOD_RESULT native_plus_arr_arr METHOD_PARAMS {
	*result = make_array(ARG_LEN(0) + ARG_LEN(1));
	memcpy(ARRAY_ITEMS(*result)+0, ARG_DATA_PTR(0), sizeof(VALUE)*ARG_LEN(0));
	memcpy(ARRAY_ITEMS(*result)+ARG_LEN(0), OBJ_DATA_PTR(argv[1]), sizeof(VALUE)*ARG_LEN(1));
	return METHOD_OK;
}

METHOD_RESULT native_push_arr_any METHOD_PARAMS { array_push(argv[0], argv[1]); METHOD_RETURN(argv[0]); }
METHOD_RESULT native_pop_arr EXT_METHOD_PARAMS {
	if(!OBJ_LEN(argv[0])) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_attribute(e, make_string("container"), argv[0]);
		set_normal_type_instance_attribute(e, make_string("key"), make_string("<last element in the array>"));
		THROW_EXCEPTION_INSTANCE(e);
	}
	*result = ARRAY_ITEMS(argv[0])[OBJ_LEN(argv[0])-1];
	OBJ_LEN(argv[0])--;
	return METHOD_OK;
}

METHOD_RESULT native_shift_arr METHOD_PARAMS { METHOD_RETURN(array_shift(argv[0])); }

METHOD_RESULT native_shift_arr_any METHOD_PARAMS {
	if(!OBJ_LEN(argv[0])) {
		METHOD_RETURN(argv[1]);
	}
	METHOD_RETURN(array_shift(argv[0]));
}

METHOD_RESULT native_index_get_arr_int_any METHOD_PARAMS {
	int idx, len;
	idx = GET_INT(argv[1]);
	if(idx < 0) {
		METHOD_RETURN(argv[2]);
	}
	len = OBJ_LEN(argv[0]);
	if(idx<len) {
		METHOD_RETURN(ARRAY_ITEMS(argv[0])[idx]);
	}
	METHOD_RETURN(argv[2]);
}

METHOD_RESULT native_index_get_arr_int EXT_METHOD_PARAMS {
	int idx, len;
	(void) ctx;
	len = OBJ_LEN(argv[0]);
	idx = GET_INT(argv[1]);
	if((idx < 0) || (idx >= len)) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_attribute(e, make_string("container"), argv[0]);
		set_normal_type_instance_attribute(e, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	*result = ARRAY_ITEMS(argv[0])[idx];
	return METHOD_OK;
}

METHOD_RESULT native_index_set_arr_int_any EXT_METHOD_PARAMS {
	int idx, len;
	(void) ctx;
	len = OBJ_LEN(argv[0]);
	idx = GET_INT(argv[1]);
	if((idx < 0) || (idx >= len)) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_attribute(e, make_string("container"), argv[0]);
		set_normal_type_instance_attribute(e, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	ARRAY_ITEMS(argv[0])[idx] = argv[2];
	METHOD_RETURN(argv[2]);
}


METHOD_RESULT native_Str_int METHOD_PARAMS {
	char s[MAX_INT_TO_STR_LEN];
	size_t len;
	len = snprintf(s, sizeof(s), "%" PRIiPTR, GET_INT(argv[0]));
	if(len >= sizeof(s)) {
		THROW_EXCEPTION("ResultTooLarge");
	}
	*result = make_string(s);
	return METHOD_OK;
}

// XXX: Not sure about this whole formatting thing
METHOD_RESULT native_Str_real METHOD_PARAMS {
	char s[MAX_REAL_TO_STR_LEN];
	char *cut_after=NULL, *p;
	size_t len;
	len = snprintf(s, sizeof(s), NGS_REAL_FMT, GET_REAL(argv[0]));
	if(len >= sizeof(s)) {
		THROW_EXCEPTION("ResultTooLarge");
	}
	p=s;
	while(1) {
		if(*p==0) {
			if(cut_after) {
				*(cut_after+1) = '\0';
			}
			break;
		}
		if(*p!='0') {
			cut_after=p;
		}
		if(*p=='.') {
			cut_after++;
		}
		p++;
	}

	*result = make_string(s);
	return METHOD_OK;
}

METHOD_RESULT native_Real_int METHOD_PARAMS {
	METHOD_RETURN(make_real((NGS_REAL) GET_INT(argv[0])));
}

METHOD_RESULT native_is_any_type METHOD_PARAMS {
	SET_BOOL(*result, obj_is_of_type(argv[0], argv[1]));
	return METHOD_OK;
}

METHOD_RESULT native_Bool_any METHOD_PARAMS {
	// printf("Bool()\n");
	// dump(argv[0]);
	if(IS_BOOL(argv[0])) METHOD_RETURN(argv[0])
	if(IS_INT(argv[0])) METHOD_RETURN(MAKE_BOOL(GET_INT(argv[0])))
	if(IS_STRING(argv[0]) || IS_ARRAY(argv[0]) || IS_HASH(argv[0])) METHOD_RETURN(MAKE_BOOL(OBJ_LEN(argv[0])));
	if(IS_NULL(argv[0])) METHOD_RETURN(MAKE_BOOL(0));
	return METHOD_ARGS_MISMATCH;
}

METHOD_RESULT native_in_any_hash METHOD_PARAMS {
	SET_BOOL(*result, get_hash_key(argv[1], argv[0]));
	return METHOD_OK;
}

METHOD_RESULT native_hash_any METHOD_PARAMS {
	// METHOD_RETURN(MAKE_INT(hash(argv[0])));
	SET_INT(*result, hash(argv[0]));
	return METHOD_OK;
}

METHOD_RESULT native_keys_hash METHOD_PARAMS {
	size_t i;
	HASH_OBJECT_ENTRY *e;
	*result = make_array(OBJ_LEN(argv[0]));
	for(e=HASH_HEAD(argv[0]), i=0; e; e=e->insertion_order_next, i++) {
		*(VALUE *)(ARRAY_ITEMS(*result)+i) = e->key;
	}
	return METHOD_OK;
}

METHOD_RESULT native_values_hash METHOD_PARAMS {
	size_t i;
	HASH_OBJECT_ENTRY *e;
	*result = make_array(OBJ_LEN(argv[0]));
	for(e=HASH_HEAD(argv[0]), i=0; e; e=e->insertion_order_next, i++) {
		*(VALUE *)(ARRAY_ITEMS(*result)+i) = e->val;
	}
	return METHOD_OK;
}

METHOD_RESULT native_update_hash_hash METHOD_PARAMS { update_hash(argv[0], argv[1]); METHOD_RETURN(argv[0]); }

METHOD_RESULT native_len METHOD_PARAMS {
	*result = MAKE_INT(OBJ_LEN(argv[0]));
	return METHOD_OK;
}

METHOD_RESULT native_index_get_hash_any_any METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e;
	e = get_hash_key(argv[0], argv[1]);
	if(!e) {
		METHOD_RETURN(argv[2]);
	}
	*result = e->val;
	return METHOD_OK;
}

METHOD_RESULT native_index_get_hash_any EXT_METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e;
	(void) ctx;
	e = get_hash_key(argv[0], argv[1]);
	if(!e) {
		VALUE exc;
		exc = make_normal_type_instance(vm->KeyNotFound);
		set_normal_type_instance_attribute(exc, make_string("container"), argv[0]);
		set_normal_type_instance_attribute(exc, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	*result = e->val;
	return METHOD_OK;
}

METHOD_RESULT native_index_set_hash_any_any METHOD_PARAMS {
	set_hash_key(argv[0], argv[1], argv[2]);
	*result = argv[2];
	return METHOD_OK;
}

METHOD_RESULT native_index_del_hash_any METHOD_PARAMS {
	del_hash_key(argv[0], argv[1]);
	*result = argv[0];
	return METHOD_OK;
}

// TODO: locking for dlerror?
// TODO: Support other dlopen() flags?
METHOD_RESULT native_CLib_str METHOD_PARAMS {
	VALUE v;
	CLIB_OBJECT *o;
	void *out;
	out = dlopen(obj_to_cstring(argv[0]), RTLD_NOW);
	if(!out) {
		fprintf(stderr, "dlopen() failed: %s\n", dlerror());
		assert(0=="Fail to dlopen()");
	}
	o = NGS_MALLOC(sizeof(*o));
	assert(o);
	o->base.type.num = T_CLIB;
	o->base.val.ptr = out;
	o->name = argv[0];
	SET_OBJ(v, o);
	METHOD_RETURN(v);
}

METHOD_RESULT native_in_str_clib METHOD_PARAMS {
	METHOD_RETURN(MAKE_BOOL(dlsym(OBJ_DATA_PTR(argv[1]), obj_to_cstring(argv[0]))));
}

METHOD_RESULT native_index_get_clib_str METHOD_PARAMS {
	VALUE v;
	CSYM_OBJECT *o;
	o = NGS_MALLOC(sizeof(*o));
	assert(o);
	o->base.type.num = T_CSYM;
	o->base.val.ptr = dlsym(OBJ_DATA_PTR(argv[0]), obj_to_cstring(argv[1]));
	assert(o->base.val.ptr);
	o->lib = argv[0];
	o->name = argv[1];
	SET_OBJ(v, o);
	METHOD_RETURN(v);
}

// OPEN(2)
// TODO: support more flags
// TODO: support providing mode instead of hard-coded 0666
METHOD_RESULT native_c_open_str_str METHOD_PARAMS {
	const char *pathname = obj_to_cstring(argv[0]);
	const char *flags_str = obj_to_cstring(argv[1]);
	int flags = 0;
	if(!flags && !strcmp(flags_str, "r"))  { flags = O_RDONLY; }
	if(!flags && !strcmp(flags_str, "w"))  { flags = O_WRONLY | O_CREAT | O_TRUNC; }
	if(!flags && !strcmp(flags_str, "a"))  { flags = O_WRONLY | O_CREAT | O_APPEND; }
	// if(!flags && !strcmp(flags_str, "rw")) { flags = O_RDWR; }
	SET_INT(*result, open(pathname, flags, 0666)); // Mode same as in bash
	return METHOD_OK;
}

// READ(2)
// TODO: error handling support
METHOD_RESULT native_c_read_int_int METHOD_PARAMS {
	// Params: fd, count
	char *buf;
	size_t count = GET_INT(argv[1]);
	ssize_t len;
	assert(count <= SSIZE_MAX);
	buf = NGS_MALLOC_ATOMIC(count);
	assert(buf);
	len = read(GET_INT(argv[0]), buf, count);

	*result = make_array(2);
	ARRAY_ITEMS(*result)[0] = MAKE_INT(len);
	if(len >= 0) {
		ARRAY_ITEMS(*result)[1] = make_string_of_len(buf, len);
	} else {
		ARRAY_ITEMS(*result)[1] = (VALUE){.num=V_NULL};
	}

	METHOD_RETURN(*result);
}

// WRITE(2)
// TODO: error handling support
METHOD_RESULT native_c_write_int_str METHOD_PARAMS { METHOD_RETURN(MAKE_INT(write(GET_INT(argv[0]), OBJ_DATA_PTR(argv[1]), OBJ_LEN(argv[1])))); }

// DUP2(2)
METHOD_RESULT native_c_dup2 METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(dup2(GET_INT(argv[0]), GET_INT(argv[1]))));
}

METHOD_RESULT native_c_lseek_int_int_str EXT_METHOD_PARAMS {
	off_t offset;
	const char *whence_str = obj_to_cstring(argv[2]);
	int whence = 0;
	(void) ctx;
	if(!strcmp(whence_str, "set")) {
		whence = SEEK_SET;
	} else {
		if(!strcmp(whence_str, "cur")) {
			whence = SEEK_CUR;
		} else {
			if(!strcmp(whence_str, "end")) {
				whence = SEEK_END;
			} else {
				VALUE exc;
				exc = make_normal_type_instance(vm->InvalidArgument);
				set_normal_type_instance_attribute(exc, make_string("which"), make_string("Third parameter to c_lseek(), 'whence'"));
				set_normal_type_instance_attribute(exc, make_string("given"), argv[2]);
				// TODO: Array of expected values maybe?
				set_normal_type_instance_attribute(exc, make_string("expected"), make_string("One of: 'set', 'cur', 'end'"));
				THROW_EXCEPTION_INSTANCE(exc);
			}
		}
	}
	offset = lseek(GET_INT(argv[0]), GET_INT(argv[1]), whence);
	METHOD_RETURN(MAKE_INT(offset));
}

METHOD_RESULT native_c_isatty METHOD_PARAMS { METHOD_RETURN(MAKE_INT(isatty(GET_INT(argv[0])))); }

METHOD_RESULT native_c_close_int METHOD_PARAMS { METHOD_RETURN(MAKE_INT(close(GET_INT(argv[0])))); }

METHOD_RESULT native_c_exit_int METHOD_PARAMS { (void)result; exit(GET_INT(argv[0])); }

METHOD_RESULT native_eq_str_str METHOD_PARAMS {
	size_t len;
	if(OBJ_LEN(argv[0]) != OBJ_LEN(argv[1])) { METHOD_RETURN(MAKE_BOOL(0)); }
	if(OBJ_DATA_PTR(argv[0]) == OBJ_DATA_PTR(argv[1])) { METHOD_RETURN(MAKE_BOOL(1)); }
	len = OBJ_LEN(argv[0]);
	METHOD_RETURN(MAKE_BOOL(!bcmp(OBJ_DATA_PTR(argv[0]), OBJ_DATA_PTR(argv[1]), len)));
}

METHOD_RESULT native_pos_str_str_int METHOD_PARAMS {
	void *p;
	int start = GET_INT(argv[2]);
	p = ngs_memmem((char *)OBJ_DATA_PTR(argv[0])+start, OBJ_LEN(argv[0])-start, OBJ_DATA_PTR(argv[1]), OBJ_LEN(argv[1]));
	if(p) {
		METHOD_RETURN(MAKE_INT(p - OBJ_DATA_PTR(argv[0])));
	}
	METHOD_RETURN(MAKE_NULL);
}

// TODO: LookupFail-child-type exceptions?
METHOD_RESULT native_index_get_str_range EXT_METHOD_PARAMS {
	size_t len;
	VALUE start, end, exc;
	(void) ctx;
	if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(argv[1])) < 2) return METHOD_ARGS_MISMATCH;
	start = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[0];
	if(!IS_INT(start)) return METHOD_ARGS_MISMATCH;
	if(GET_INT(start) < 0) {
		exc = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_attribute(exc, make_string("message"), make_string("Negative range start when calling [](s:Str, r:Range)"));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	if(((size_t) GET_INT(start)) > OBJ_LEN(argv[0])) {
		exc = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_attribute(exc, make_string("message"), make_string("Range starts after string end when calling [](s:Str, r:Range)"));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	end = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[1];
	if(IS_NULL(end)) {
		len = OBJ_LEN(argv[0]) - GET_INT(start);
		*result = make_string_of_len(NULL, len);
		memcpy(OBJ_DATA_PTR(*result), OBJ_DATA_PTR(argv[0]) + GET_INT(start), len);
		return METHOD_OK;
	}
	if(!IS_INT(end)) return METHOD_ARGS_MISMATCH;
	if(GET_INT(end) < GET_INT(start)) {
		exc = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_attribute(exc, make_string("message"), make_string("Range end smaller than range start when calling [](s:Str, r:Range)"));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	len = GET_INT(end) - GET_INT(start);
	if(obj_is_of_type(argv[1], vm->InclusiveRange)) {
		len++;
	}
	if(GET_INT(start) + len > OBJ_LEN(argv[0])) {
		exc = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_attribute(exc, make_string("message"), make_string("Range ends after string end [](s:Str, r:Range)"));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	*result = make_string_of_len(NULL, len);

	memcpy(OBJ_DATA_PTR(*result), OBJ_DATA_PTR(argv[0]) + GET_INT(start), len);
	return METHOD_OK;
}

METHOD_RESULT native_eq_bool_bool METHOD_PARAMS { METHOD_RETURN(MAKE_BOOL(argv[0].num == argv[1].num)); }
METHOD_RESULT native_not_bool METHOD_PARAMS { METHOD_RETURN(MAKE_BOOL(argv[0].num == V_FALSE)); }

// XXX: glibc specific fmemopen()
METHOD_RESULT native_compile_str_str EXT_METHOD_PARAMS {
	ast_node *tree = NULL;
	char *bytecode;
	size_t len;
	yycontext yyctx;
	int parse_ok;
	(void) ctx;
	memset(&yyctx, 0, sizeof(yycontext));
	yyctx.fail_pos = -1;
	yyctx.fail_rule = "(unknown)";
	yyctx.lines = 0;
	yyctx.lines_postions[0] = 0;
	// printf("PT 1 %p\n", OBJ_DATA_PTR(argv[0]));
	yyctx.input_file = fmemopen(OBJ_DATA_PTR(argv[0]), OBJ_LEN(argv[0]), "r");
	parse_ok = yyparse(&yyctx);
	if(!parse_ok) {
		// TODO: error message and/or exception
		VALUE exc;
		char *err = NGS_MALLOC_ATOMIC(1024);
		exc = make_normal_type_instance(vm->CompileFail);
		set_normal_type_instance_attribute(exc, make_string("given"), argv[0]);
		snprintf(err, 1024, "Failed to parse at position %d (%s), rule %s", yyctx.fail_pos, sprintf_position(&yyctx, yyctx.fail_pos), yyctx.fail_rule);
		set_normal_type_instance_attribute(exc, make_string("info"), make_string(err));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	tree = yyctx.__;
	IF_DEBUG(COMPILER, print_ast(tree, 0);)
	yyrelease(&yyctx);
	bytecode = compile(tree, obj_to_cstring(argv[1]), &len);
	// BROKEN SINCE BYTECODE FORMAT CHANGE // IF_DEBUG(COMPILER, decompile(bytecode, 0, len);)
	METHOD_RETURN(make_string_of_len(bytecode, len));
}

METHOD_RESULT native_load_str_str EXT_METHOD_PARAMS {
	size_t ip;
	(void) ctx;
	ip = vm_load_bytecode(vm, OBJ_DATA_PTR(argv[0]));
	METHOD_RETURN(make_closure_obj(ip, 0, 0, 0, 0, 0, NULL));
}

METHOD_RESULT native_decode_json_str EXT_METHOD_PARAMS {
	METHOD_RESULT mr;
	(void) ctx;
	mr = decode_json(argv[0], result);
	if(mr == METHOD_EXCEPTION) {
		VALUE exc;
		// TODO: more specific error
		exc = make_normal_type_instance(vm->Error);
		set_normal_type_instance_attribute(exc, make_string("message"), *result);
		set_normal_type_instance_attribute(exc, make_string("backtrace"), make_backtrace(vm, ctx));
		*result = exc;
	}
	return mr;
}

METHOD_RESULT native_encode_json_obj EXT_METHOD_PARAMS {
	METHOD_RESULT mr;
	(void) ctx;
	mr = encode_json(argv[0], result);
	if(mr == METHOD_EXCEPTION) {
		VALUE exc;
		// TODO: more specific error
		exc = make_normal_type_instance(vm->Error);
		// could be big... // set_normal_type_instance_attribute(exc, make_string("data"), argv[0]);
		set_normal_type_instance_attribute(exc, make_string("message"), *result);
		set_normal_type_instance_attribute(exc, make_string("backtrace"), make_backtrace(vm, ctx));
		*result = exc;
	}
	return mr;
}

METHOD_RESULT native_backtrace EXT_METHOD_PARAMS { (void) argv; METHOD_RETURN(make_backtrace(vm, ctx)); }
METHOD_RESULT native_resolve_ip EXT_METHOD_PARAMS { (void) ctx; METHOD_RETURN(resolve_ip(vm, GET_INT(argv[0]))); }

METHOD_RESULT native_globals EXT_METHOD_PARAMS {
	VALUE ret;
	size_t i;
	(void) ctx;
	(void) argv;

	ret = make_hash(256);
	for(i=0; i<vm->globals_len; i++) {
		if(IS_NOT_UNDEF(vm->globals[i])) {
			set_hash_key(ret, make_string(vm->globals_names[i]), vm->globals[i]);
		}
	}

	METHOD_RETURN(ret);
}

METHOD_RESULT native_type_str METHOD_PARAMS { METHOD_RETURN(make_normal_type(argv[0])); }
METHOD_RESULT native_typeof_any METHOD_PARAMS {
	if(IS_NORMAL_TYPE_INSTANCE(argv[0])) {
		METHOD_RETURN(NORMAL_TYPE_INSTANCE_TYPE(argv[0]));
	}
	// XXX: Not implemented yet
	METHOD_RETURN(MAKE_NULL);
}
METHOD_RESULT native_get_attr_nti_str EXT_METHOD_PARAMS {
	// WARNING: for now get_normal_type_instace_attribute can only throw AttrNotFound
	//          if it changes in future the calling convention below should be changed
	//          The reason for such calling convention is not to pass the VM to
	//          get_normal_type_instace_attribute() just so it will have access to the
	//          exceptions.
	METHOD_RESULT mr;
	(void) ctx;
	mr = get_normal_type_instace_attribute(argv[0], argv[1], result);
	if(mr == METHOD_EXCEPTION) {
		VALUE exc;
		exc = make_normal_type_instance(vm->AttrNotFound);
		set_normal_type_instance_attribute(exc, make_string("container"), argv[0]);
		set_normal_type_instance_attribute(exc, make_string("key"), argv[1]);
		*result = exc;
	}
	return mr;
}
METHOD_RESULT native_set_attr_nti_str_any METHOD_PARAMS { set_normal_type_instance_attribute(argv[0], argv[1], argv[2]); METHOD_RETURN(argv[2]); }
METHOD_RESULT native_inherit_nt_nt METHOD_PARAMS { add_normal_type_inheritance(argv[0], argv[1]); METHOD_RETURN(argv[0]); }

// Consider moving to obj.c
// Maybe return METHOD_ARGS_MISMATCH insted of InvalidArgument?
METHOD_RESULT native_join_arr_str EXT_METHOD_PARAMS {
	size_t i, len=OBJ_LEN(argv[0]), dst_len, l, sep_l;
	char *p, *sep_p;
	(void) ctx;
	if(!len) {
		METHOD_RETURN(make_string(""));
	}
	for(i=0, dst_len=0; i<len; i++) {
		if(!IS_STRING(ARRAY_ITEMS(argv[0])[i])) {
			VALUE exc;
			exc = make_normal_type_instance(vm->InvalidArgument);
			set_normal_type_instance_attribute(exc, make_string("message"), make_string("join - array must contains only strings"));
			set_normal_type_instance_attribute(exc, make_string("given"), ARRAY_ITEMS(argv[0])[i]);
			THROW_EXCEPTION_INSTANCE(exc);
		}
		dst_len += OBJ_LEN(ARRAY_ITEMS(argv[0])[i]);
	}
	dst_len += (OBJ_LEN(argv[1]) * (len-1));

	*result = make_string_of_len(NULL, dst_len);
	OBJ_LEN(*result) = dst_len;
	p = OBJ_DATA_PTR(*result);

	l = OBJ_LEN(ARRAY_ITEMS(argv[0])[0]);
	memcpy(p, OBJ_DATA_PTR(ARRAY_ITEMS(argv[0])[0]), l);
	p += l;

	sep_p = OBJ_DATA_PTR(argv[1]);
	sep_l = OBJ_LEN(argv[1]);

	for(i=1; i<len; i++) {
		memcpy(p, sep_p, sep_l);
		p += sep_l;

		l = OBJ_LEN(ARRAY_ITEMS(argv[0])[i]);
		memcpy(p, OBJ_DATA_PTR(ARRAY_ITEMS(argv[0])[i]), l);
		p += l;
	}
	return METHOD_OK;
}

METHOD_RESULT native_copy_arr METHOD_PARAMS { METHOD_RETURN(make_array_with_values(OBJ_LEN(argv[0]), ARRAY_ITEMS(argv[0]))); }

METHOD_RESULT native_c_fork METHOD_PARAMS {
	(void) argv;
	pid_t pid;
	pid = fork();
	METHOD_RETURN(MAKE_INT(pid));
}

METHOD_RESULT native_c_waitpid METHOD_PARAMS {
	VALUE ret;
	pid_t pid;
	int status;
	pid = waitpid(GET_INT(argv[0]), &status, 0);
	ret = make_array(2);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(pid);
	ARRAY_ITEMS(ret)[1] = MAKE_INT(status);
	METHOD_RETURN(ret);
}

// TODO: dedup native_get_attr_bt_str / native_get_attr_nt_str
METHOD_RESULT native_get_attr_bt_str EXT_METHOD_PARAMS {
	VALUE exc;
	char *attr = obj_to_cstring(argv[1]);
	(void) ctx;
	if(!strcmp(attr, "constructors")) {
		// dump_titled("constructors", NGS_TYPE_CONSTRUCTORS(argv[0]));
		METHOD_RETURN(NGS_TYPE_CONSTRUCTORS(argv[0]));
	}
	if(!strcmp(attr, "name")) {
		// dump_titled("constructors", NGS_TYPE_CONSTRUCTORS(argv[0]));
		METHOD_RETURN(NGS_TYPE_NAME(argv[0]));
	}

	exc = make_normal_type_instance(vm->AttrNotFound);
	set_normal_type_instance_attribute(exc, make_string("container"), argv[0]);
	set_normal_type_instance_attribute(exc, make_string("key"), argv[1]);
	*result = exc;
	return METHOD_EXCEPTION;
}

METHOD_RESULT native_get_attr_nt_str EXT_METHOD_PARAMS {
	VALUE exc;
	char *attr = obj_to_cstring(argv[1]);
	(void) ctx;
	if(!strcmp(attr, "constructors")) {
		// dump_titled("constructors", NGS_TYPE_CONSTRUCTORS(argv[0]));
		METHOD_RETURN(NGS_TYPE_CONSTRUCTORS(argv[0]));
	}
	if(!strcmp(attr, "name")) {
		// dump_titled("constructors", NGS_TYPE_CONSTRUCTORS(argv[0]));
		METHOD_RETURN(NGS_TYPE_NAME(argv[0]));
	}

	exc = make_normal_type_instance(vm->AttrNotFound);
	set_normal_type_instance_attribute(exc, make_string("container"), argv[0]);
	set_normal_type_instance_attribute(exc, make_string("key"), argv[1]);
	*result = exc;
	return METHOD_EXCEPTION;
}

METHOD_RESULT native_c_pipe METHOD_PARAMS {
	VALUE ret;
	int status;
	int pipefd[2];
	(void) argv;
	status = pipe(pipefd);
	ret = make_array(3);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(status);
	ARRAY_ITEMS(ret)[1] = MAKE_INT(pipefd[0]);
	ARRAY_ITEMS(ret)[2] = MAKE_INT(pipefd[1]);
	METHOD_RETURN(ret);
}

METHOD_RESULT native_c_dup2_int_int METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(dup2(GET_INT(argv[0]), GET_INT(argv[1]))));
}

METHOD_RESULT native_get_c_errno METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(MAKE_INT(errno));
}

METHOD_RESULT native_c_execve METHOD_PARAMS {
	char *exec_filename;
	char **exec_argv, **exec_envp;
	exec_filename = obj_to_cstring(argv[0]);
	exec_argv = obj_to_cstring_array(argv[1]);
	exec_envp = obj_to_cstring_array(argv[2]);
	METHOD_RETURN(MAKE_INT(execve(exec_filename, exec_argv, exec_envp)));
}

METHOD_RESULT native_C_WEXITSTATUS METHOD_PARAMS { METHOD_RETURN(MAKE_INT(WEXITSTATUS(GET_INT(argv[0])))); }
METHOD_RESULT native_C_WTERMSIG METHOD_PARAMS { METHOD_RETURN(MAKE_INT(WTERMSIG(GET_INT(argv[0])))); }

GLOBAL_VAR_INDEX check_global_index(VM *vm, const char *name, size_t name_len, int *found) {
	VAR_INDEX *var;
	HASH_FIND(hh, vm->globals_indexes, name, name_len, var);
	if(var) {
		*found = 1;
		return var->index;
	}
	*found = 0;
	return 0;
}

METHOD_RESULT native_same_any_any METHOD_PARAMS {
	if(argv[0].num & TAG_AND) {
		METHOD_RETURN(MAKE_BOOL(argv[0].num == argv[1].num));
	}
	METHOD_RETURN(MAKE_BOOL(argv[0].ptr == argv[1].ptr));
}

METHOD_RESULT native_attrs_closure METHOD_PARAMS {
	METHOD_RETURN(CLOSURE_OBJ_ATTRS(argv[0]));
}

METHOD_RESULT native_attrs_closure_any METHOD_PARAMS {
	CLOSURE_OBJ_ATTRS(argv[0]) = argv[1];
	METHOD_RETURN(argv[1]);
}

// TODO: consider returning Hash instead of Arr
VALUE _native_params(VALUE *params, int n_params_required, int n_params_optional, int params_flags) {
	// See vm_call()
	VALUE ret;
	VALUE param;
	int have_arr_splat = params_flags & PARAMS_FLAG_ARR_SPLAT;
	int have_hash_splat = params_flags & PARAMS_FLAG_HASH_SPLAT;
	int i;
	ret = make_array(0);
	for(i=0; i < n_params_required; i++) {
		param = make_hash(2);
		set_hash_key(param, make_string("name"), params[i*2 + 0]);
		set_hash_key(param, make_string("type"), params[i*2 + 1]);
		array_push(ret, param);
	}
	for(i = n_params_required; i < n_params_required + n_params_optional; i++) {
		param = make_hash(4);
		set_hash_key(param, make_string("name"), params[n_params_required*2 + (i-n_params_required)*3 + 0]);
		set_hash_key(param, make_string("type"), params[n_params_required*2 + (i-n_params_required)*3 + 1]);
		set_hash_key(param, make_string("dflt"), params[n_params_required*2 + (i-n_params_required)*3 + 2]);
		array_push(ret, param);
	}
	i = n_params_required*2 + n_params_optional*3;
	if(have_arr_splat) {
		param = make_hash(4);
		set_hash_key(param, make_string("name"), params[i + 0]);
		set_hash_key(param, make_string("type"), params[i + 1]);
		set_hash_key(param, make_string("splat"), make_string("*"));
		array_push(ret, param);
		i+=2;
	}
	if(have_hash_splat) {
		param = make_hash(1);
		set_hash_key(param, make_string("name"), params[i + 0]);
		set_hash_key(param, make_string("type"), params[i + 1]);
		set_hash_key(param, make_string("splat"), make_string("**"));
		array_push(ret, param);
	}
	return ret;
}

#define callable (argv[0])
METHOD_RESULT native_params_closure METHOD_PARAMS {
	// See vm_call()
	METHOD_RETURN(_native_params(CLOSURE_OBJ_PARAMS(callable), CLOSURE_OBJ_N_REQ_PAR(callable), CLOSURE_OBJ_N_OPT_PAR(callable), CLOSURE_OBJ_PARAMS_FLAGS(callable)));
}
#undef callable

METHOD_RESULT native_attrs_nm METHOD_PARAMS {
	METHOD_RETURN(NATIVE_METHOD_ATTRS(argv[0]));
}

METHOD_RESULT native_attrs_nm_any METHOD_PARAMS {
	NATIVE_METHOD_ATTRS(argv[0]) = argv[1];
	METHOD_RETURN(argv[1]);
}

#define callable (argv[0])
METHOD_RESULT native_params_nm METHOD_PARAMS {
	METHOD_RETURN(_native_params(
		NATIVE_METHOD_OBJ_PARAMS(callable),
		NATIVE_METHOD_OBJ_N_REQ_PAR(callable),
		NATIVE_METHOD_OBJ_N_OPT_PAR(callable),
		0
	));
}
#undef callable

// TODO: Something more efficient than obj_to_cstring. Maybe use strncasecmp?
METHOD_RESULT native_c_strcasecmp METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(strcasecmp(obj_to_cstring(argv[0]), obj_to_cstring(argv[1]))));
}

GLOBAL_VAR_INDEX get_global_index(VM *vm, const char *name, size_t name_len) {
	VAR_INDEX *var;
	GLOBAL_VAR_INDEX index;
	int found;
	DEBUG_VM_RUN("entering get_global_index() vm=%p name=%.*s\n", vm, (int)name_len, name);
	index = check_global_index(vm, name, name_len, &found);
	if(found) {
		DEBUG_VM_RUN("leaving get_global_index() status=found vm=%p name=%.*s -> index=" GLOBAL_VAR_INDEX_FMT "\n", vm, (int)name_len, name, index);
		return index;
	}
	assert(vm->globals_len < (MAX_GLOBALS-1));
	var = NGS_MALLOC(sizeof(*var));
	var->name = NGS_MALLOC(name_len);
	memcpy(var->name, name, name_len);
	var->index = vm->globals_len++;
	HASH_ADD_KEYPTR(hh, vm->globals_indexes, var->name, name_len, var);
	GLOBALS[var->index] = MAKE_UNDEF;
	vm->globals_names[var->index] = var->name;
	DEBUG_VM_RUN("leaving get_global_index() status=new vm=%p name=%.*s -> index=" GLOBAL_VAR_INDEX_FMT "\n", vm, (int)name_len, name, var->index);
	return var->index;
}

void register_global_func(VM *vm, int pass_extra_params, char *name, void *func_ptr, int argc, ...) {
	size_t index;
	int i;
	va_list varargs;
	NATIVE_METHOD_OBJECT *o;
	VALUE *argv = NULL;
	o = NGS_MALLOC(sizeof(*o));
	o->base.type.num = T_NATIVE_METHOD;
	o->base.val.ptr = func_ptr;
	o->params.n_params_required = argc;
	o->params.n_params_optional = 0; /* currently none of builtins uses optional parameters */
	o->pass_extra_params = pass_extra_params;
	vm->last_doc_hash = make_hash(4);
	o->attrs = make_hash(8);
		set_hash_key(o->attrs, make_string("name"), make_string(name));
		set_hash_key(o->attrs, make_string("doc"), vm->last_doc_hash);
	if(argc) {
		argv = NGS_MALLOC(argc * sizeof(VALUE) * 2);
		assert(argv);
		va_start(varargs, argc);
		for(i=0; i<argc; i++) {
			// name:
			argv[i*2+0] = make_string(va_arg(varargs, char *));
			// type:
			argv[i*2+1] = (VALUE){.ptr = va_arg(varargs, NGS_TYPE *)};
		}
		va_end(varargs);
	}
	o->params.params = argv;
	index = get_global_index(vm, name, strlen(name));
	if(IS_ARRAY(GLOBALS[index])) {
		array_push(GLOBALS[index], MAKE_OBJ(o));
		return;
	}
	if(IS_NGS_TYPE(GLOBALS[index])) {
		array_push(NGS_TYPE_CONSTRUCTORS(GLOBALS[index]), MAKE_OBJ(o));
		return;
	}
	if(IS_UNDEF(GLOBALS[index])) {
		GLOBALS[index] = make_array_with_values(1, &MAKE_OBJ(o));
		return;
	}
	assert(0 == "register_global_func fail");
}

// TODO: consider array values (for sepatate lines or list items)
void _doc(VM *vm, char *k, char *v) {
	set_hash_key(vm->last_doc_hash, make_string(k), make_string(v));
}

void set_global(VM *vm, const char *name, VALUE v) {
	size_t index;
	index = get_global_index(vm, name, strlen(name));
	GLOBALS[index] = v;
}

NGS_TYPE *register_builtin_type(VM *vm, const char *name, IMMEDIATE_TYPE native_type_id) {
	size_t index;
	VALUE t;
	t = make_normal_type(make_string(name));
	// Fixes for built-ins - start
	NGS_TYPE_ID(t) = native_type_id;
	OBJ_LEN(NGS_TYPE_CONSTRUCTORS(t)) = 0;
	// Fixes for built-ins - end
	index = get_global_index(vm, name, strlen(name));
	assert(IS_UNDEF(GLOBALS[index]));
	GLOBALS[index] = t;
	return t.ptr;
}

void vm_init(VM *vm, int argc, char **argv) {
	char **env, *equal_sign;
	VALUE env_hash, k, v;
	VALUE argv_array;
	int i;
	vm->bytecode = NULL;
	vm->bytecode_len = 0;
	vm->globals_indexes = NULL; // UT_hash_table
	vm->globals_len = 0;
	vm->globals = NGS_MALLOC(sizeof(*(vm->globals)) * MAX_GLOBALS);
	vm->globals_names = NGS_MALLOC(sizeof(char *) * MAX_GLOBALS);
	vm->regions = NULL;
	vm->regions_len = 0;
	vm->regions_allocated = 0;
	// Keep global functions registration in order.
	// This way the compiler can use globals_indexes as the beginning of
	// it's symbol table for globals.
	vm->Null = register_builtin_type(vm, "Null", T_NULL);
	vm->Bool = register_builtin_type(vm, "Bool", T_BOOL);
	vm->Int  = register_builtin_type(vm, "Int",  T_INT);
	vm->Real = register_builtin_type(vm, "Real", T_REAL);
	vm->Str  = register_builtin_type(vm, "Str",  T_STR);
	vm->Arr  = register_builtin_type(vm, "Arr",  T_ARR);
	vm->Fun  = register_builtin_type(vm, "Fun",  T_FUN);
		vm->Closure  = register_builtin_type(vm, "Closure",  T_CLOSURE);
		vm->NativeMethod  = register_builtin_type(vm, "NativeMethod",  T_NATIVE_METHOD);
	vm->Any  = register_builtin_type(vm, "Any",  T_ANY);
		vm->BasicTypeInstance  = register_builtin_type(vm, "BasicTypeInstance",  T_BASICTI);
		vm->NormalTypeInstance = register_builtin_type(vm, "NormalTypeInstance", T_NORMTI);
	vm->Seq  = register_builtin_type(vm, "Seq",  T_SEQ);
	vm->Type = register_builtin_type(vm, "Type", T_TYPE);
		vm->BasicType  = register_builtin_type(vm, "BasicType",  T_BASICT);
		vm->NormalType = register_builtin_type(vm, "NormalType", T_NORMT);
	vm->Hash = register_builtin_type(vm, "Hash", T_HASH);
	vm->CLib = register_builtin_type(vm, "CLib", T_CLIB);
	vm->CSym = register_builtin_type(vm, "CSym", T_CSYM);

#define MKTYPE(name) \
	VALUE name; \
	name = make_normal_type(make_string(#name)); \
	set_global(vm, #name, name); \
	vm->name = name;

#define MKSUBTYPE(name, parent) \
	MKTYPE(name); \
	add_normal_type_inheritance(name, parent);

#define SETUP_RANGE_TYPE(name) \
	set_hash_key(NGS_TYPE_FIELDS(name), make_string("start"), MAKE_INT(0)); \
	set_hash_key(NGS_TYPE_FIELDS(name), make_string("end"), MAKE_INT(1));

	MKTYPE(Exception);
		MKSUBTYPE(Error, Exception);
			MKSUBTYPE(LookupFail, Error);
				MKSUBTYPE(KeyNotFound, LookupFail);
				MKSUBTYPE(IndexNotFound, LookupFail);
				MKSUBTYPE(AttrNotFound, LookupFail);
				MKSUBTYPE(GlobalNotFound, LookupFail);
			MKSUBTYPE(InvalidArgument, Error);
			MKSUBTYPE(CompileFail, Error);
			MKSUBTYPE(CallFail, Error);
				MKSUBTYPE(DontKnowHowToCall, CallFail);
				MKSUBTYPE(ImplNotFound, CallFail);
			MKSUBTYPE(SwitchFail, Error);

	MKTYPE(Backtrace);

	MKTYPE(Command);
	MKTYPE(Redir);


	// XXX: changing NGS_TYPE_FIELDS of InclusiveRange or ExclusiveRange
	//      in such a way that "start" is not 0 or "end" is not 1
	//      will break everything. TODO: make sure this can not be done by
	//      an NGS script.
	MKTYPE(Range);

		MKSUBTYPE(InclusiveRange, Range);
		SETUP_RANGE_TYPE(InclusiveRange);

		MKSUBTYPE(ExclusiveRange, Range);
		SETUP_RANGE_TYPE(ExclusiveRange);

#undef SETUP_RANGE_TYPE
#undef MKSUBTYPE
#undef MKTYPE

	vm->eqeq = make_array(0);
	set_global(vm, "==", vm->eqeq);


	// CLib and c calls
	register_global_func(vm, 0, "CLib",     &native_CLib_str,          1, "name",   vm->Str);
	register_global_func(vm, 0, "in",       &native_in_str_clib,       2, "symbol", vm->Str, "lib", vm->CLib);
	register_global_func(vm, 0, "[]",       &native_index_get_clib_str,2, "lib",    vm->CLib,"symbol", vm->Str);

	// Native methods
	register_global_func(vm, 0, "attrs",    &native_attrs_nm,          1, "m",      vm->NativeMethod);
	_doc(vm, "", "Gets native method attribues. Usually a Hash with name and doc keys.");
	register_global_func(vm, 0, "attrs",    &native_attrs_nm_any,      2, "m",      vm->NativeMethod, "datum", vm->Any);
	_doc(vm, "", "Sets native method attribues. Should be a Hash.");
	register_global_func(vm, 0, "params",   &native_params_nm,         1, "m",      vm->NativeMethod);

	// Closure
	register_global_func(vm, 0, "==",       &native_same_any_any,      2, "a",      vm->Closure, "b", vm->Closure);
	register_global_func(vm, 0, "attrs",    &native_attrs_closure,     1, "c",      vm->Closure);
	register_global_func(vm, 0, "attrs",    &native_attrs_closure_any, 2, "c",      vm->Closure, "datum", vm->Any);
	register_global_func(vm, 0, "params",   &native_params_closure,    1, "c",      vm->Closure);

	// Real
	register_global_func(vm, 0, "+",        &native_plus_real_real,      2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "*",        &native_mul_real_real,       2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "/",        &native_div_real_real,       2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "-",        &native_minus_real_real,     2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "<",        &native_less_real_real,      2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "<=",       &native_less_eq_real_real,   2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, ">",        &native_greater_real_real,   2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, ">=",       &native_greater_eq_real_real,2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "==",       &native_eq_real_real,        2, "a",   vm->Real, "b", vm->Real);
	register_global_func(vm, 0, "Str",      &native_Str_real,            1, "r",   vm->Real);
	register_global_func(vm, 0, "Real",     &native_Real_int,            1, "n",   vm->Int);

	// BasicType
	register_global_func(vm, 1, ".",        &native_get_attr_bt_str,       2, "obj", vm->BasicType,          "attr", vm->Str);

	// NormalType
	register_global_func(vm, 1, ".",        &native_get_attr_nt_str,       2, "obj", vm->NormalType,         "attr", vm->Str);
	register_global_func(vm, 1, ".",        &native_get_attr_nti_str,      2, "obj", vm->NormalTypeInstance, "attr", vm->Str);
	register_global_func(vm, 0, ".=",       &native_set_attr_nti_str_any,  3, "obj", vm->NormalTypeInstance, "attr", vm->Str, "v", vm->Any);
	register_global_func(vm, 0, "inherit",  &native_inherit_nt_nt,         2, "t",   vm->NormalType,         "parent", vm->NormalType);

	// Type
	register_global_func(vm, 0, "Type",     &native_type_str          ,1, "name",   vm->Str);
	register_global_func(vm, 0, "typeof",   &native_typeof_any        ,1, "x",      vm->Any);
	_doc(vm, "", "Returns type of the given instance");
	_doc(vm, "x", "Instance (an object)");

	// low level file operations
	register_global_func(vm, 0, "c_dup2",   &native_c_dup2_int_int,    2, "oldfd",    vm->Int, "newfd", vm->Int);
	register_global_func(vm, 0, "c_open",   &native_c_open_str_str,    2, "pathname", vm->Str, "flags", vm->Str);
	_doc(vm, "flags", "r - O_RDONLY; w - O_WRONLY | O_CREAT | O_TRUNC; a - O_WRONLY | O_CREAT | O_APPEND");
	register_global_func(vm, 0, "c_close",  &native_c_close_int,       1, "fd",       vm->Int);
	register_global_func(vm, 0, "c_read",   &native_c_read_int_int,    2, "fd",       vm->Int, "count", vm->Int);
	register_global_func(vm, 0, "c_write",  &native_c_write_int_str,   2, "fd",       vm->Int, "s",     vm->Str);
	register_global_func(vm, 0, "c_dup2",   &native_c_dup2,            2, "oldfd",    vm->Int, "newfd", vm->Int);
	register_global_func(vm, 1, "c_lseek",  &native_c_lseek_int_int_str,3,"fd",       vm->Int, "offset", vm->Int, "whence", vm->Str);
	_doc(vm, "whence", "One of: set, cur, end");
	register_global_func(vm, 0, "c_isatty", &native_c_isatty,           1,"fd",       vm->Int);

	// low level misc
	register_global_func(vm, 0, "c_exit",   &native_c_exit_int,        1, "status",   vm->Int);
	register_global_func(vm, 0, "c_fork",   &native_c_fork,            0);
	register_global_func(vm, 0, "c_pipe",   &native_c_pipe,            0);
	register_global_func(vm, 0, "c_waitpid",&native_c_waitpid,         1, "pid",      vm->Int);
	register_global_func(vm, 0, "c_execve", &native_c_execve,          3, "filename", vm->Str, "argv", vm->Arr, "envp", vm->Arr);
	register_global_func(vm, 0, "C_WEXITSTATUS", &native_C_WEXITSTATUS,1, "status",   vm->Int);
	register_global_func(vm, 0, "C_WTERMSIG", &native_C_WTERMSIG,      1, "status",   vm->Int);

	register_global_func(vm, 0, "get_c_errno", &native_get_c_errno,    0);

	register_global_func(vm, 0, "c_strcasecmp", &native_c_strcasecmp,  2, "a",   vm->Str,  "b", vm->Str);

	// boolean
	register_global_func(vm, 0, "==",       &native_eq_bool_bool,      2, "a",   vm->Bool, "b", vm->Bool);
	register_global_func(vm, 0, "not",      &native_not_bool,          1, "x",   vm->Bool);

	// array
	register_global_func(vm, 0, "+",        &native_plus_arr_arr,      2, "a",   vm->Arr, "b", vm->Arr);
	register_global_func(vm, 0, "push",     &native_push_arr_any,      2, "arr", vm->Arr, "v", vm->Any);
	register_global_func(vm, 1, "pop",      &native_pop_arr,           1, "arr", vm->Arr);
	register_global_func(vm, 0, "shift",    &native_shift_arr,         1, "arr", vm->Arr);
	register_global_func(vm, 0, "shift",    &native_shift_arr_any,     2, "arr", vm->Arr, "dflt", vm->Any);
	register_global_func(vm, 0, "len",      &native_len,               1, "arr", vm->Arr);
	register_global_func(vm, 0, "get",      &native_index_get_arr_int_any, 3, "arr", vm->Arr, "idx", vm->Int, "dflt", vm->Any);
	register_global_func(vm, 1, "[]",       &native_index_get_arr_int, 2, "arr", vm->Arr, "idx", vm->Int);
	register_global_func(vm, 1, "[]=",      &native_index_set_arr_int_any, 3, "arr", vm->Arr, "idx", vm->Int, "v", vm->Any);
	register_global_func(vm, 1, "join",     &native_join_arr_str,      2, "arr", vm->Arr, "s", vm->Str);
	register_global_func(vm, 0, "copy",     &native_copy_arr,          1, "arr", vm->Arr);

	// string
	// TODO: other string comparison operators
	register_global_func(vm, 0, "len",      &native_len,               1, "s",   vm->Str);
	register_global_func(vm, 0, "==",       &native_eq_str_str,        2, "a",   vm->Str, "b", vm->Str);
	register_global_func(vm, 0, "pos",      &native_pos_str_str_int,   3, "haystack", vm->Str, "needle", vm->Str, "start", vm->Int);
	register_global_func(vm, 1, "[]",       &native_index_get_str_range, 2, "s", vm->Str, "range", vm->Range);

	// int
	register_global_func(vm, 0, "+",        &native_plus_int_int,      2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "*",        &native_mul_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "/",        &native_div_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "%",        &native_mod_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "-",        &native_minus_int_int,     2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "<",        &native_less_int_int,      2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "<=",       &native_less_eq_int_int,   2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, ">",        &native_greater_int_int,   2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, ">=",       &native_greater_eq_int_int,2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, 0, "==",       &native_eq_int_int,        2, "a",   vm->Int, "b", vm->Int);

	// misc
	register_global_func(vm, 0, "===",      &native_same_any_any,      2, "a",   vm->Any, "b", vm->Any);
	register_global_func(vm, 0, "dump",     &native_dump_any,          1, "obj", vm->Any);
	register_global_func(vm, 0, "echo",     &native_echo_str,          1, "s",   vm->Str);
	register_global_func(vm, 0, "Bool",     &native_Bool_any,          1, "x",   vm->Any);
	register_global_func(vm, 0, "Str",      &native_Str_int,           1, "n",   vm->Int);
	register_global_func(vm, 0, "is",       &native_is_any_type,       2, "obj", vm->Any, "t", vm->Type);
	register_global_func(vm, 1, "compile",  &native_compile_str_str,   2, "code",vm->Str, "fname", vm->Str);
	register_global_func(vm, 1, "load",     &native_load_str_str,      2, "bytecode", vm->Str, "func_name", vm->Str);
	register_global_func(vm, 1, "decode_json",&native_decode_json_str, 1, "s", vm->Str);
	register_global_func(vm, 1, "encode_json",&native_encode_json_obj, 1, "obj", vm->Any);
	register_global_func(vm, 1, "Backtrace",&native_backtrace,         0);
	register_global_func(vm, 1, "resolve_ip",&native_resolve_ip,       1, "ip", vm->Int);
	register_global_func(vm, 1, "globals",  &native_globals,           0);

	// hash
	register_global_func(vm, 0, "in",       &native_in_any_hash,       2, "x",   vm->Any, "h", vm->Hash);
	register_global_func(vm, 0, "hash",     &native_hash_any,          1, "x",   vm->Any);
	register_global_func(vm, 0, "keys",     &native_keys_hash,         1, "h",   vm->Hash);
	register_global_func(vm, 0, "values",   &native_values_hash,       1, "h",   vm->Hash);
	register_global_func(vm, 0, "update",   &native_update_hash_hash,  2, "dst", vm->Hash, "src", vm->Hash);
	register_global_func(vm, 0, "len",      &native_len,               1, "h",   vm->Hash);
	register_global_func(vm, 0, "get",      &native_index_get_hash_any_any,    3, "h",   vm->Hash,"k", vm->Any, "dflt", vm->Any);
	register_global_func(vm, 1, "[]",       &native_index_get_hash_any,        2, "h",   vm->Hash,"k", vm->Any);
	register_global_func(vm, 0, "[]=",      &native_index_set_hash_any_any,    3, "h",   vm->Hash,"k", vm->Any, "v", vm->Any);
	register_global_func(vm, 0, "del",      &native_index_del_hash_any,        2, "h",   vm->Hash,"k", vm->Any);

	// http://stackoverflow.com/questions/3473692/list-environment-variables-with-c-in-unix
	env_hash = make_hash(32);
	for (env = environ; *env; ++env) {
		equal_sign = strchr(*env, '=');
		if(equal_sign) {
			// should be there but ...
			k = make_string_of_len(*env, equal_sign-*env);
			v = make_string(equal_sign+1);
			set_hash_key(env_hash, k, v);
		}
	}
	set_global(vm, "ENV", env_hash);

	argv_array = make_array(argc);
	for(i=0; i<argc; i++) {
		ARRAY_ITEMS(argv_array)[i] = make_string(argv[i]);
	}
	set_global(vm, "ARGV", argv_array);
	set_global(vm, "impl_not_found_hook", vm->impl_not_found_hook = make_array(0)); // There must be a catch-all in stdlib
	set_global(vm, "global_not_found_hook", vm->global_not_found_hook = make_array(0));
	set_global(vm, "init", vm->init = make_array(0));

	// TODO: Some good solution for many defines
#define E(name) set_global(vm, "C_" #name, MAKE_INT(name))
	// errno -ls | awk '{print "E("$1");"}' | xargs -n10
	E(EPERM); E(ENOENT); E(ESRCH); E(EINTR); E(EIO); E(ENXIO); E(E2BIG); E(ENOEXEC); E(EBADF); E(ECHILD);
	E(EAGAIN); E(ENOMEM); E(EACCES); E(EFAULT); E(ENOTBLK); E(EBUSY); E(EEXIST); E(EXDEV); E(ENODEV); E(ENOTDIR);
	E(EISDIR); E(EINVAL); E(ENFILE); E(EMFILE); E(ENOTTY); E(ETXTBSY); E(EFBIG); E(ENOSPC); E(ESPIPE); E(EROFS);
	E(EMLINK); E(EPIPE); E(EDOM); E(ERANGE); E(EDEADLK); E(ENAMETOOLONG); E(ENOLCK); E(ENOSYS); E(ENOTEMPTY); E(ELOOP);
	E(EWOULDBLOCK); E(ENOMSG); E(EIDRM); E(ECHRNG); E(EL2NSYNC); E(EL3HLT); E(EL3RST); E(ELNRNG); E(EUNATCH); E(ENOCSI);
	E(EL2HLT); E(EBADE); E(EBADR); E(EXFULL); E(ENOANO); E(EBADRQC); E(EBADSLT); E(EDEADLOCK); E(EBFONT); E(ENOSTR);
	E(ENODATA); E(ETIME); E(ENOSR); E(ENONET); E(ENOPKG); E(EREMOTE); E(ENOLINK); E(EADV); E(ESRMNT); E(ECOMM);
	E(EPROTO); E(EMULTIHOP); E(EDOTDOT); E(EBADMSG); E(EOVERFLOW); E(ENOTUNIQ); E(EBADFD); E(EREMCHG); E(ELIBACC); E(ELIBBAD);
	E(ELIBSCN); E(ELIBMAX); E(ELIBEXEC); E(EILSEQ); E(ERESTART); E(ESTRPIPE); E(EUSERS); E(ENOTSOCK); E(EDESTADDRREQ); E(EMSGSIZE);
	E(EPROTOTYPE); E(ENOPROTOOPT); E(EPROTONOSUPPORT); E(ESOCKTNOSUPPORT); E(EOPNOTSUPP); E(EPFNOSUPPORT); E(EAFNOSUPPORT); E(EADDRINUSE); E(EADDRNOTAVAIL); E(ENETDOWN);
	E(ENETUNREACH); E(ENETRESET); E(ECONNABORTED); E(ECONNRESET); E(ENOBUFS); E(EISCONN); E(ENOTCONN); E(ESHUTDOWN); E(ETOOMANYREFS); E(ETIMEDOUT);
	E(ECONNREFUSED); E(EHOSTDOWN); E(EHOSTUNREACH); E(EALREADY); E(EINPROGRESS); E(ESTALE); E(EUCLEAN); E(ENOTNAM); E(ENAVAIL); E(EISNAM);
	E(EREMOTEIO); E(EDQUOT); E(ENOMEDIUM); E(EMEDIUMTYPE); E(ECANCELED); E(ENOKEY); E(EKEYEXPIRED); E(EKEYREVOKED); E(EKEYREJECTED); E(EOWNERDEAD);
	E(ENOTRECOVERABLE); E(ERFKILL); E(EHWPOISON); E(ENOTSUP);
#undef E

}

void ctx_init(CTX *ctx) {
	ctx->stack_ptr = 0;
	ctx->frame_ptr = 0;
	// XXX: correct sizeof?
	memset(ctx->stack, 0, sizeof(ctx->stack));
	memset(ctx->frames, 0, sizeof(ctx->frames));
}

size_t vm_load_bytecode(VM *vm, char *bc) {

	// For BYTECODE_SECTION_TYPE_CODE
	BYTECODE_HANDLE *bytecode;
	BYTECODE_SECTION_TYPE type;
	BYTECODE_SECTION_LEN len;
	BYTECODE_SECTIONS_COUNT i;
	char *data;
	char *p;

	// For BYTECODE_SECTION_TYPE_GLOBALS
	BYTECODE_GLOBALS_COUNT g, g_max;
	BYTECODE_GLOBALS_LOC_COUNT l, l_max;
	BYTECODE_GLOBALS_OFFSET o;
	GLOBAL_VAR_INDEX gvi;
	unsigned char global_name_len;
	char global_name[257];

	size_t ip = 0;
	DEBUG_VM_API("vm_load_bytecode() VM=%p bytecode=%p\n", vm, bc);

	VM_REGION *region = NULL;


	bytecode = ngs_start_unserializing_bytecode(bc);


	for(i=0; i<bytecode->sections_count; i++) {
		ngs_fetch_bytecode_section(bytecode, &type, &len, &data);
		switch(type) {
			case BYTECODE_SECTION_TYPE_CODE:

				NGS_MALLOC_OBJ(region);
				ENSURE_ARRAY_ROOM(vm->regions, vm->regions_allocated, vm->regions_len, 8);
				PUSH_ARRAY_ELT(vm->regions, vm->regions_len, *region);
				region = &vm->regions[vm->regions_len-1];
				region->start_ip = vm->bytecode_len;
				region->len = len;
				region->source_tracking_entries_count = 0;
				region->source_tracking_entries = NULL;

				assert(data[len-1] == OP_RET);
				if(vm->bytecode) {
					vm->bytecode = NGS_REALLOC(vm->bytecode, vm->bytecode_len + len);
				} else {
					vm->bytecode = NGS_MALLOC(len);
				}
				assert(vm->bytecode);
				memcpy(vm->bytecode + vm->bytecode_len, data, len);
				ip = vm->bytecode_len;
				vm->bytecode_len += len;
				break;
			case BYTECODE_SECTION_TYPE_GLOBALS:
				p = data;
				BYTECODE_GET(g_max, p, BYTECODE_GLOBALS_COUNT);
				for(g=0; g<g_max; g++) {
					BYTECODE_GET(global_name_len, p, unsigned char); // XXX - check what happens with len 128 and more (unsigned char = char)
					assert(global_name_len);
					memcpy(global_name, p, global_name_len);
					global_name[global_name_len] = 0;
					p += global_name_len;
					BYTECODE_GET(l_max, p, BYTECODE_GLOBALS_LOC_COUNT);
					for(l=0; l<l_max; l++) {
						BYTECODE_GET(o, p, BYTECODE_GLOBALS_OFFSET);
						gvi = get_global_index(vm, global_name, global_name_len);
						DEBUG_VM_API("vm_load_bytecode() processing global patching num=%i name=%s offset=%i resolved_index=%i\n", g, global_name, o, gvi);
						*(GLOBAL_VAR_INDEX *)(&vm->bytecode[ip + o]) = gvi;
					}
				}
				break;
			case BYTECODE_SECTION_TYPE_SRCLOC:
				// TODO: types
				{
					uint16_t i, files_names_count;
					uint8_t file_name_len;
					char *file_name;

					uint32_t entries_count;

					assert(region); // SRCLOC section must come after code section, which allocated region
					assert(!region->source_tracking_entries); // At most one SRCLOC per code section
					assert(!region->files_names); // At most one SRCLOC per code section


					p = data;
					BYTECODE_GET(files_names_count, p, uint16_t);
					for(i=0; i<files_names_count; i++) {
						// PUSH_ARRAY_ELT(vm->regions, vm->regions_len, *region);
						BYTECODE_GET(file_name_len, p, uint8_t);
						file_name = NGS_MALLOC_ATOMIC(file_name_len+1);
						memcpy(file_name, p, file_name_len);
						file_name[file_name_len] = '\0';
						p += file_name_len;
						ENSURE_ARRAY_ROOM(region->files_names, region->files_names_allocated, region->files_names_len, 8);
						PUSH_ARRAY_ELT(region->files_names, region->files_names_len, file_name);
					}

					BYTECODE_GET(entries_count, p, uint32_t);
					region->source_tracking_entries_count = entries_count;
					region->source_tracking_entries = NGS_MALLOC(entries_count * sizeof(source_tracking_entry));
					memcpy(region->source_tracking_entries, p, entries_count * sizeof(source_tracking_entry));
				}
				break;
			default:
				// Types 0-255 are a must. Types above that are optional.
				if(type < 0x100) {
					assert(0 == "vm_load_bytecode(): Unknown section type");
				}
		}
	}
	return ip;
}

// XXX: Factor out to "define"s access to parameters. Coupling to this data structure is all over.
#define HAVE_KWARGS_MARKER ((argc >= 2) && IS_KWARGS_MARKER(argv[argc-1]))
METHOD_RESULT vm_call(VM *vm, CTX *ctx, VALUE *result, const VALUE callable, int argc, const VALUE *argv) {
	LOCAL_VAR_INDEX lvi;
	int i;
	METHOD_RESULT mr;
	VALUE *callable_items;

	// dump_titled("CALLABLE", callable);
	// if(argc) {
	// 	dump_titled("ARGV0", argv[0]);
	// }
	if(IS_ARRAY(callable)) {
		for(i=OBJ_LEN(callable)-1, callable_items = OBJ_DATA_PTR(callable); i>=0; i--) {
			mr = vm_call(vm, ctx, result, callable_items[i], argc, argv);
			if((mr == METHOD_OK) || (mr == METHOD_EXCEPTION) || (mr == METHOD_IMPL_MISSING)) {
				return mr;
			}
			assert(mr == METHOD_ARGS_MISMATCH);
			if(mr != METHOD_ARGS_MISMATCH) {
				dump_titled("RESULT", *result);
				VALUE exc;
				// TODO: Appropriate exception type, not Exception
				exc = make_normal_type_instance(vm->Exception);
				set_normal_type_instance_attribute(exc, make_string("message"), make_string("Internal: mr != METHOD_ARGS_MISMATCH"));
				set_normal_type_instance_attribute(exc, make_string("callable"), callable_items[i]);
				set_normal_type_instance_attribute(exc, make_string("args"), make_array_with_values(argc, argv));
				THROW_EXCEPTION_INSTANCE(exc);
			}
		}
		// --- impl_not_found_hook() - start ---
		if(THIS_FRAME.do_call_impl_not_found_hook) {
			// impl_not_found_hook == [] when stdlib is not loaded (-E bootstrap switch / during basic tests)
			if(OBJ_LEN(vm->impl_not_found_hook)) {
				VALUE new_argv;
				new_argv = make_array(argc+1);
				ARRAY_ITEMS(new_argv)[0] = callable;
				memcpy(&ARRAY_ITEMS(new_argv)[1], argv, sizeof(VALUE)*argc);
				THIS_FRAME.do_call_impl_not_found_hook = 0;
				// last_ip should have been already set up before calling vm_call()
				mr = vm_call(vm, ctx, result, vm->impl_not_found_hook, argc+1, ARRAY_ITEMS(new_argv));
				THIS_FRAME.do_call_impl_not_found_hook = 1;
				if((mr == METHOD_OK) || (mr == METHOD_EXCEPTION)) {
					return mr;
				}
				assert(mr == METHOD_IMPL_MISSING);
			}
			// Either we called impl_not_found_hook and it resulted METHOD_IMPL_MISSING
			// or we don't have impl_not_found_hook
			VALUE exc;
			exc = make_normal_type_instance(vm->ImplNotFound);
			set_normal_type_instance_attribute(exc, make_string("callable"), callable);
			set_normal_type_instance_attribute(exc, make_string("args"), make_array_with_values(argc, argv));
			THROW_EXCEPTION_INSTANCE(exc);
		}
		// --- impl_not_found_hook() - end ---
		return METHOD_IMPL_MISSING;
	}

	if(IS_NATIVE_METHOD(callable)) {
		// None of native method uses optional parameters for now
		if(NATIVE_METHOD_OBJ_N_OPT_PAR(callable)) {
			assert(0=="Optional parameters for native methods are not implemented yet");
		}
		if(HAVE_KWARGS_MARKER) {
			return METHOD_ARGS_MISMATCH;
		}
		// dump_titled("Native callable", callable);
		if(argc != NATIVE_METHOD_OBJ_N_REQ_PAR(callable)) {
			return METHOD_ARGS_MISMATCH;
		}
		// printf("PT 0\n");
		for(lvi=0; lvi<NATIVE_METHOD_OBJ_N_REQ_PAR(callable); lvi++) {
			// TODO: make sure second argument is type durng closure creation
			// dump_titled("ARGV[lvi]", argv[lvi]);
			if(!obj_is_of_type(argv[lvi], NATIVE_METHOD_OBJ_PARAMS(callable)[lvi*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}
		// printf("PT 2\n");
		if(NATIVE_METHOD_EXTRA_PARAMS(callable)) {
			mr = ((VM_EXT_FUNC)OBJ_DATA_PTR(callable))(vm, ctx, argv, result);
		} else {
			mr = ((VM_FUNC)OBJ_DATA_PTR(callable))(argv, result);
		}
		return mr;
	}

	if(IS_CLOSURE(callable)) {
		int n_params_required = CLOSURE_OBJ_N_REQ_PAR(callable);
		int n_params_optional = CLOSURE_OBJ_N_OPT_PAR(callable);
		int have_arr_splat = CLOSURE_OBJ_PARAMS_FLAGS(callable) & PARAMS_FLAG_ARR_SPLAT;
		int have_hash_splat = CLOSURE_OBJ_PARAMS_FLAGS(callable) & PARAMS_FLAG_HASH_SPLAT;
		int have_kwargs = HAVE_KWARGS_MARKER;
		int n_kwargs_used = 0;
		int i, j;
		VALUE kw, *params;
		HASH_OBJECT_ENTRY *e;
		VALUE named_arguments[MAX_ARGS];

		// TODO: handle (exception?) keyword arguments providing required parameter which was also given as positional argument

		if(have_kwargs) {
			kw = argv[argc-2];
			argc -= 2;
		} else {
			kw = MAKE_UNDEF;
		}

		if(argc < n_params_required) {
			// Some of the required parameters missing, might be in keyword arguments
			if(!have_kwargs) {
				return METHOD_ARGS_MISMATCH;
			}
			if(argc + (int)OBJ_LEN(kw) < n_params_required) {
				// Not enough of keyword arguments to provide all required parameters
				return METHOD_ARGS_MISMATCH;
			}
			// We have some required parameters missing which might me in the keyword arguments
		}

		if((argc > n_params_required + n_params_optional) && (!have_arr_splat)) {
			return METHOD_ARGS_MISMATCH;
		}

		// Check the required arguments given as positional arguments
		params = CLOSURE_OBJ_PARAMS(callable);
		j = MIN(argc, n_params_required);
		for(i=0; i<j; i++) {
			if(!obj_is_of_type(argv[i], params[i*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}

		// Check the required arguments given as keyword arguments
		if(argc < n_params_required) {
			// We have some required parameters missing which might me in the keyword arguments
			assert(n_params_required < MAX_ARGS);
			for(i=argc; i<n_params_required; i++) {
				e = get_hash_key(kw, params[i*2 + 0]);
				if(!e) {
					// Required parameter is not in keyword arguments
					return METHOD_ARGS_MISMATCH;
				}
				if(!obj_is_of_type(e->val, params[i*2+1])) {
					return METHOD_ARGS_MISMATCH;
				}
				named_arguments[i] = e->val;
				n_kwargs_used++;
			}
		}

		// Check optional parameters given as positional arguments
		j = MIN(argc, n_params_required + n_params_optional);
		for(i=n_params_required; i < j; i++) {
			if(!obj_is_of_type(argv[i], params[n_params_required*2 + (i-n_params_required)*3 + 1])) {
				return METHOD_ARGS_MISMATCH;
			}
			named_arguments[i] = params[n_params_required*2 + (i-n_params_required)*3 + 2];
		}

		// Check optional parameters given as keyword arguments
		i = MAX(j, n_params_required);
		j = n_params_required + n_params_optional;
		for(; i<j; i++) {
			if(!IS_UNDEF(kw)) {
				e = get_hash_key(kw, params[n_params_required*2 + (i-n_params_required)*3 + 0]);
			} else {
				e = NULL;
			}
			if(!e) {
				// Required parameter is not in keyword arguments, use default value
				named_arguments[i] = params[n_params_required*2 + (i-n_params_required)*3+2];
				continue;
			}
			if(!obj_is_of_type(e->val, params[n_params_required*2 + (i-n_params_required)*3+1])) {
				return METHOD_ARGS_MISMATCH;
			}
			named_arguments[i] = e->val;
			n_kwargs_used++;
		}

		if((!have_hash_splat) && (!IS_UNDEF(kw)) && (n_kwargs_used < (int)OBJ_LEN(kw))) {
			return METHOD_ARGS_MISMATCH;
		}

		if(CLOSURE_OBJ_N_LOCALS(callable)) {
			ctx->frames[ctx->frame_ptr].locals = NGS_MALLOC(CLOSURE_OBJ_N_LOCALS(callable) * sizeof(VALUE));
			j = MIN(argc, n_params_required + n_params_optional);
			memcpy(ctx->frames[ctx->frame_ptr].locals, argv, (j + have_arr_splat + have_hash_splat) * sizeof(*argv));
			// TODO: memcpy?
			for(i = j; i < n_params_required + n_params_optional; i++) {
				ctx->frames[ctx->frame_ptr].locals[i] = named_arguments[i];
			}
			if(have_arr_splat) {
				ctx->frames[ctx->frame_ptr].locals[n_params_required + n_params_optional] = make_array_with_values(argc - j, &argv[j]);
			}
			if(have_hash_splat) {
				ctx->frames[ctx->frame_ptr].locals[n_params_required + n_params_optional + have_arr_splat] = IS_UNDEF(kw) ? make_hash(0) : kw;
			}
			for(i = n_params_required + n_params_optional + have_arr_splat + have_hash_splat; i < CLOSURE_OBJ_N_LOCALS(callable); i++) {
				ctx->frames[ctx->frame_ptr].locals[i] = MAKE_UNDEF;
			}
		} else {
			ctx->frames[ctx->frame_ptr].locals = NULL;
		}

		ctx->frames[ctx->frame_ptr].closure = callable;
		ctx->frames[ctx->frame_ptr].try_info_ptr = 0;
		ctx->frames[ctx->frame_ptr].do_call_impl_not_found_hook = 1;
		ctx->frames[ctx->frame_ptr].last_ip = 0;
		ctx->frame_ptr++;
		mr = vm_run(vm, ctx, CLOSURE_OBJ_IP(callable), result);
		ctx->frame_ptr--;
		return mr;
	}

	if(IS_NGS_TYPE(callable)) {
		return vm_call(vm, ctx, result, NGS_TYPE_CONSTRUCTORS(callable), argc, argv);
	}

	if(IS_NORMAL_TYPE_CONSTRUCTOR(callable)) {
		*result = make_normal_type_instance(NORMAL_TYPE_CONSTRUCTOR_TYPE(callable));
		// init() is optional when constructor is called without arguments
		// --- init() - start ---
		VALUE v;
		VALUE *new_argv;
		if(argc) {
			new_argv = NGS_MALLOC((argc + 1) * sizeof(*new_argv));
			new_argv[0] = *result;
			memcpy(&new_argv[1], argv, argc * sizeof(*argv));
		} else {
			new_argv = result;
		}
		THIS_FRAME.do_call_impl_not_found_hook = 0;
		mr = vm_call(vm, ctx, &v, vm->init, argc+1, new_argv);
		THIS_FRAME.do_call_impl_not_found_hook = 1;
		if(argc && (mr == METHOD_IMPL_MISSING)) {
			VALUE exc;
			exc = make_normal_type_instance(vm->ImplNotFound);
			set_normal_type_instance_attribute(exc, make_string("message"), make_string("Normal type constructor: init() not found"));
			set_normal_type_instance_attribute(exc, make_string("callable"), vm->init);
			set_normal_type_instance_attribute(exc, make_string("args"), make_array_with_values(argc, new_argv));
			THROW_EXCEPTION_INSTANCE(exc);
		}
		if(mr == METHOD_EXCEPTION) {
			*result = v;
			return mr;
		}
		if((mr == METHOD_OK) || (mr == METHOD_IMPL_MISSING)) {
			return METHOD_OK;
		}
		assert(0 == "Unexpected init() result");
		// --- init() - end ---
	}

	VALUE exc;
	exc = make_normal_type_instance(vm->DontKnowHowToCall);
	set_normal_type_instance_attribute(exc, make_string("callable"), callable);
	set_normal_type_instance_attribute(exc, make_string("args"), make_array_with_values(argc, argv));
	THROW_EXCEPTION_INSTANCE(exc);
}
#undef HAVE_KWARGS_MARKER

METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result) {
	VALUE v, callable, command, *v_ptr;
	VAR_LEN_OBJECT *vlo;
	int i;
	unsigned char opcode;
	GLOBAL_VAR_INDEX gvi;
	PATCH_OFFSET po;
	JUMP_OFFSET jo;
	LOCAL_VAR_INDEX lvi;
	size_t vlo_len, j;
	METHOD_RESULT mr;
	size_t saved_stack_ptr = ctx->stack_ptr;
	size_t string_components_count;

	// for OP_MAKE_CLOSURE
	LOCAL_VAR_INDEX n_locals, n_params_required, n_params_optional;
	UPVAR_INDEX n_uplevels, uvi;
	int params_flags;

main_loop:
	opcode = vm->bytecode[ip++];
#ifdef DO_NGS_DEBUG
	if(opcode <= sizeof(opcodes_names) / sizeof(char *)) {
		DEBUG_VM_RUN("main_loop FRAME_PTR=%zu IP=%zu OP=%s STACK_LEN=%zu TRY_LEN=%i\n", ctx->frame_ptr, ip-1, opcodes_names[opcode], ctx->stack_ptr, THIS_FRAME.try_info_ptr);
		decompile(vm->bytecode, ip-1, ip);
		for(j=ctx->stack_ptr; j>0; j--) {
			printf("Stack @ %zu\n", j-1);
			dump(ctx->stack[j-1]);
		}
	}
#endif

	// Guidelines
	// * increase ip as soon as arguments extraction is finished
	switch(opcode) {
		case OP_HALT:
							goto end_main_loop;
		case OP_PUSH_NULL:
							PUSH(MAKE_NULL);
							goto main_loop;
		case OP_PUSH_FALSE:
							PUSH(MAKE_FALSE);
							goto main_loop;
		case OP_PUSH_TRUE:
							PUSH(MAKE_TRUE);
							goto main_loop;
		case OP_PUSH_INT:
							// Arg: n
							// In ...
							// Out: ... n
							// TODO: make it push_intSIZE maybe?
							i = *(int *) &vm->bytecode[ip];
							ip += sizeof(i);
							PUSH(MAKE_INT(i));
							goto main_loop;
		case OP_PUSH_REAL:
							// Arg: n
							// In ...
							// Out: ... n
							// TODO: make it push_intSIZE maybe?
							PUSH(make_real(*(NGS_REAL *) &vm->bytecode[ip]));
							ip += sizeof(NGS_REAL);
							goto main_loop;
		case OP_PUSH_L_STR:
							// Arg: LEN + string
							// In: ...
							// Out: ... string
							// printf("LSTR @ %p\n", &vm->bytecode[ip]);
							vlo = NGS_MALLOC(sizeof(*vlo));
							vlo->len = (size_t) vm->bytecode[ip];
							vlo->base.type.num = T_STR;
							vlo->base.val.ptr = NGS_MALLOC_ATOMIC(vlo->len);
							memcpy(vlo->base.val.ptr, &(vm->bytecode[ip+1]), vlo->len);
							ip += 1 + vm->bytecode[ip];
							SET_OBJ(v, vlo);
							PUSH(v);
							goto main_loop;
		case OP_DUP:
							DUP;
							goto main_loop;
		case OP_POP:
							REMOVE_TOP;
							goto main_loop;
		case OP_XCHG:
							EXPECT_STACK_DEPTH(2);
							v = TOP;
							TOP = SECOND;
							SECOND = v;
							goto main_loop;
		case OP_RESOLVE_GLOBAL:
							POP(v);
							assert(IS_STRING(v));
							PUSH_NOCHECK(MAKE_INT(get_global_index(vm, OBJ_DATA_PTR(v), OBJ_LEN(v))));
							goto main_loop;
		case OP_PATCH:
							// Arg: offset
							// In ... n
							// Out: ...
							// Effect: bytecode[offset] <- n
							POP(v);
							ARG(po, PATCH_OFFSET);
#ifdef DO_NGS_DEBUG
							DEBUG_VM_RUN("OP_PATCH dst_idx=%zu v=%d\n", ip+po, *(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po]);
							assert(*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] == 0); // try to catch patching at invalid offset
#endif
							*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] = GET_INT(v);
							goto main_loop;
		case OP_FETCH_GLOBAL:
							ARG_GVI;
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_FETCH_GLOBAL gvi=%d len=%d\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
#endif
							// TODO: report error here instead of crashing
							if(IS_UNDEF(GLOBALS[gvi])) {
								THIS_FRAME.last_ip = ip;

								THIS_FRAME.do_call_impl_not_found_hook = 0;
								// last_ip should have been already set up before calling vm_call()
								v = make_string(vm->globals_names[gvi]);
								mr = vm_call(vm, ctx, result, vm->global_not_found_hook, 1, &v);
								THIS_FRAME.do_call_impl_not_found_hook = 1;
								if(IS_UNDEF(GLOBALS[gvi])) {
									VALUE exc;
									exc = make_normal_type_instance(vm->GlobalNotFound);
									set_normal_type_instance_attribute(exc, make_string("name"), make_string(vm->globals_names[gvi]));
									set_normal_type_instance_attribute(exc, make_string("index"), MAKE_INT(gvi));
									set_normal_type_instance_attribute(exc, make_string("backtrace"), make_backtrace(vm, ctx));
									if(mr == METHOD_EXCEPTION) {
										set_normal_type_instance_attribute(exc, make_string("cause"), *result);
									} else {
										if (mr != METHOD_IMPL_MISSING) {
											set_normal_type_instance_attribute(exc, make_string("info"), make_string("Additionally, global_not_found_hook() failed to provide the global"));
										}
									}
									*result = exc;
									goto exception;
								}
							}
							// dump_titled("FETCH_GLOBAL", GLOBALS[gvi]);
							PUSH(GLOBALS[gvi]);
							goto main_loop;
		case OP_STORE_GLOBAL:
							ARG_GVI;
							EXPECT_STACK_DEPTH(1);
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_STORE_GLOBAL gvi=%d len=%zu\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
#endif
							GLOBALS[gvi] = TOP;
							REMOVE_TOP;
							goto main_loop;
		case OP_FETCH_LOCAL:
							ARG_LVI;
							assert(IS_NOT_UNDEF(LOCALS[lvi]));
							PUSH(LOCALS[lvi]);
							// printf("LVI %d FRAME_PTR %d\n", lvi, ctx->frame_ptr-1);
							// dump_titled("OP_FETCH_LOCAL", LOCALS[lvi]);
							goto main_loop;
		case OP_STORE_LOCAL:
							ARG_LVI;
							POP(LOCALS[lvi]);
							goto main_loop;
		case OP_CALL:
							// TODO: print arguments of failed call, not just the callable
							// In (current): ... result_placeholder (null), arg1, ..., argN, argc, callable
							// Out: ... result
							EXPECT_STACK_DEPTH(2);
							POP_NOCHECK(callable);
							POP_NOCHECK(v); // number of arguments
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-GET_INT(v)-1], callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							// assert(ctx->stack[ctx->stack_ptr-GET_INT(v)-1].num);
							if(mr == METHOD_EXCEPTION) {
								*result = ctx->stack[ctx->stack_ptr-GET_INT(v)-1];
								// dump_titled("E1", *result);
								goto exception;
							}
							if(mr != METHOD_OK) {
								// printf("MR %d\n", mr);
								// dump_titled("RESULT", *result);
								for(v_ptr=&ctx->stack[ctx->stack_ptr-GET_INT(v)];v_ptr < &ctx->stack[ctx->stack_ptr];v_ptr++) {
									dump_titled("Failed argument", *v_ptr);
								}
								dump_titled("Failed callable / 1", callable);
								assert(0=="Handling failed method calls is not implemented yet");
							}
							REMOVE_TOP_N(GET_INT(v));
							goto main_loop;
		case OP_CALL_EXC:
							// Calls exception handler, METHOD_IMPL_MISSING means we should re-throw the exception
							EXPECT_STACK_DEPTH(2);
							POP_NOCHECK(callable);
							POP_NOCHECK(v); // number of arguments
							THIS_FRAME.do_call_impl_not_found_hook = 0;
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-GET_INT(v)-1], callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							THIS_FRAME.do_call_impl_not_found_hook = 1;
							if(mr == METHOD_EXCEPTION) {
								// TODO: special handling? Exception during exception handling.
								*result = ctx->stack[ctx->stack_ptr-GET_INT(v)-1];
								goto exception;
							}
							if(mr == METHOD_IMPL_MISSING) {
								goto exception_return;
							}
							if(mr != METHOD_OK) {
								dump_titled("Failed callable / 2", callable);
								assert(0=="Handling failed method calls is not implemented yet");
							}
							REMOVE_TOP_N(GET_INT(v));
							goto main_loop;
		case OP_CALL_ARR:
							// In (current): ... result_placeholder (null), argv, callable
							// Out: ... result
							POP(callable);
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-2], callable, OBJ_LEN(ctx->stack[ctx->stack_ptr-1]), ARRAY_ITEMS(ctx->stack[ctx->stack_ptr-1]));
							// assert(ctx->stack[ctx->stack_ptr-2].num);
							if(mr == METHOD_EXCEPTION) {
								// printf("E2\n");
								*result = ctx->stack[ctx->stack_ptr-2];
								goto exception;
							}
							if(mr != METHOD_OK) {
								dump_titled("Failed argument array", ctx->stack[ctx->stack_ptr-1]);
								dump_titled("Failed callable / 3", callable);
								assert(0=="Handling failed method calls is not implemented yet");
							}
							REMOVE_TOP_N(1);
							goto main_loop;
		case OP_RET:
							if(saved_stack_ptr < ctx->stack_ptr) {
								POP(*result);
								// dump_titled("RESULT", *result);
							} else {
								// TODO: fix the compiler. Example:
								// F f() {
								// 	# x
								// }
								// f()
								assert(0=="Function does not have result value");
							}
							assert(saved_stack_ptr == ctx->stack_ptr);
							return METHOD_OK;
		case OP_JMP:
do_jump:
							ARG(jo, JUMP_OFFSET);
							ip += jo;
							goto main_loop;
		case OP_JMP_TRUE:
							POP(v);
#ifdef DO_NGS_DEBUG
							assert(IS_BOOL(v));
#endif
							if(IS_TRUE(v)) goto do_jump;
							ip += sizeof(jo);
							goto main_loop;
		case OP_JMP_FALSE:
							POP(v);
#ifdef DO_NGS_DEBUG
							assert(IS_BOOL(v));
#endif
							if(IS_FALSE(v)) goto do_jump;
							ip += sizeof(jo);
							goto main_loop;
		case OP_MAKE_ARR:
							POP(v);
							vlo_len = GET_INT(v);
							v = make_array_with_values(vlo_len, &(ctx->stack[ctx->stack_ptr-vlo_len]));
							ctx->stack_ptr -= vlo_len;
							PUSH_NOCHECK(v);
							goto main_loop;
		case OP_MAKE_CLOSURE:
							// Arg: code_jump_offset, number_of_locals
							// In: ...,
							//   arg1_name, arg1_type, ... argN_name, argN_type,
							//   argN+1_name, argN+1_type, argN+1_default_value, ... argM_name, argM_type, argM_default_value,
							//   argc_of_required_args, argc_of_optional_args
							// Out: ..., CLOSURE_OBJECT
							ARG(jo, JUMP_OFFSET);
							ARG(n_params_required, LOCAL_VAR_INDEX);
							ARG(n_params_optional, LOCAL_VAR_INDEX);
							ARG(n_locals, LOCAL_VAR_INDEX);
							ARG(n_uplevels, UPVAR_INDEX);
							ARG(params_flags, int);
							v = make_closure_obj(
									ip+jo,
									n_locals, n_params_required, n_params_optional, n_uplevels, params_flags,
									&ctx->stack[ctx->stack_ptr - (n_params_required + ADDITIONAL_PARAMS_COUNT)*2 - n_params_optional*3]
							);
							ctx->stack_ptr -= (n_params_required + ADDITIONAL_PARAMS_COUNT)*2 + n_params_optional*3;
							if(n_uplevels) {
								assert(CLOSURE_OBJ_N_UPLEVELS(THIS_FRAME_CLOSURE) >= n_uplevels-1);
								CLOSURE_OBJ_UPLEVELS(v) = NGS_MALLOC(sizeof(CLOSURE_OBJ_UPLEVELS(v)[0]) * n_uplevels);
								// First level of upvars are the local variables of current frame
								CLOSURE_OBJ_UPLEVELS(v)[0] = LOCALS;
								// Other levels come from current closure upvars
								memcpy(&(CLOSURE_OBJ_UPLEVELS(v)[1]), CLOSURE_OBJ_UPLEVELS(THIS_FRAME_CLOSURE), sizeof(CLOSURE_OBJ_UPLEVELS(v)[0]) * (n_uplevels - 1));
							}
							PUSH(v);
							goto main_loop;
		case OP_TO_STR:
							CONVERTING_OP(IS_STRING, Str);
		case OP_MAKE_STR:
							// TODO: (optimization) update top of the stack instead of POP and PUSH
							POP(v);
							string_components_count = GET_INT(v);
							v = join_strings(string_components_count, &(ctx->stack[ctx->stack_ptr-string_components_count]));
							assert(ctx->stack_ptr >= string_components_count);
							ctx->stack_ptr -= string_components_count;
							PUSH_NOCHECK(v);
							goto main_loop;
		case OP_PUSH_EMPTY_STR:
							PUSH(make_var_len_obj(T_STR, 1, 0));
							goto main_loop;
		case OP_GLOBAL_DEF_P:
							ARG_GVI;
							PUSH(MAKE_BOOL(IS_NOT_UNDEF(GLOBALS[gvi])));
							goto main_loop;
		case OP_LOCAL_DEF_P:
							ARG_LVI;
							PUSH(MAKE_BOOL(IS_NOT_UNDEF(LOCALS[lvi])));
							goto main_loop;
		case OP_DEF_GLOBAL_FUNC:
							// Arg: gvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							ARG_GVI;
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_STORE_GLOBAL gvi=%d len=%zu\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
#endif
							if(IS_UNDEF(GLOBALS[gvi])) {
								GLOBALS[gvi] = make_array_with_values(1, &TOP);
							} else {
								PUSH_FUNC(GLOBALS[gvi], TOP);
							}
							goto main_loop;
		case OP_DEF_LOCAL_FUNC:
							// Arg: lvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							ARG_LVI;
							if(IS_UNDEF(LOCALS[lvi])) {
								LOCALS[lvi] = make_array_with_values(1, &TOP);
							} else {
								PUSH_FUNC(LOCALS[lvi], TOP);
							}
							goto main_loop;
		case OP_FETCH_UPVAR:
#ifdef DO_NGS_DEBUG
							assert(ctx->frame_ptr);
#endif
							ARG_UVI;
							ARG_LVI;
							// printf("uvi=%d lvi=%d\n", uvi, lvi);
							PUSH(UPLEVELS[uvi][lvi]);
							goto main_loop;
		case OP_STORE_UPVAR:
#ifdef DO_NGS_DEBUG
							assert(ctx->frame_ptr);
#endif
							ARG_UVI;
							ARG_LVI;
							POP(v);
							UPLEVELS[uvi][lvi] = v;
							goto main_loop;
		case OP_UPVAR_DEF_P:
#ifdef DO_NGS_DEBUG
							assert(ctx->frame_ptr);
#endif
							ARG_UVI;
							ARG_LVI;
							PUSH(MAKE_BOOL(IS_NOT_UNDEF(UPLEVELS[uvi][lvi])));
							goto main_loop;
		case OP_DEF_UPVAR_FUNC:
							// XXX: untested and not covered by tests yet
#ifdef DO_NGS_DEBUG
							assert(ctx->frame_ptr);
#endif
							// Arg: uvi, lvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							ARG_UVI;
							ARG_LVI;
							if(IS_UNDEF(UPLEVELS[uvi][lvi])) {
								UPLEVELS[uvi][lvi] = make_array_with_values(1, &TOP);
							} else {
								PUSH_FUNC(UPLEVELS[uvi][lvi], TOP);
							}
							goto main_loop;
		case OP_MAKE_HASH:
							POP(v);
							vlo_len = GET_INT(v);
							v = make_hash(vlo_len);
							ctx->stack_ptr -= vlo_len * 2;
							for(j=0; j<vlo_len;j++) {
								// Use pointer maybe instead of ctx->stack[ctx->stack_ptr...]
								set_hash_key(v, ctx->stack[ctx->stack_ptr+j*2], ctx->stack[ctx->stack_ptr+j*2+1]);
							}
							PUSH_NOCHECK(v);
							goto main_loop;
		case OP_TO_BOOL:
							CONVERTING_OP(IS_BOOL, Bool);
		case OP_TO_ARR:
							CONVERTING_OP(IS_ARRAY, Arr);
		case OP_TO_HASH:
							CONVERTING_OP(IS_HASH, Hash);
		case OP_ARR_APPEND:
							EXPECT_STACK_DEPTH(2);
							array_push(ctx->stack[ctx->stack_ptr-2], ctx->stack[ctx->stack_ptr-1]);
							REMOVE_TOP_NOCHECK;
							goto main_loop;
		case OP_ARR_CONCAT:
							EXPECT_STACK_DEPTH(2);
							native_plus_arr_arr(&ctx->stack[ctx->stack_ptr-2], &v);
							REMOVE_TOP_N_NOCHECK(2);
							PUSH_NOCHECK(v);
							goto main_loop;
		case OP_GUARD:
							POP(v);
							assert(IS_BOOL(v));
							if(IS_TRUE(v)) {
								goto main_loop;
							}
							assert(saved_stack_ptr == ctx->stack_ptr);
							return METHOD_ARGS_MISMATCH;
		case OP_TRY_START:
							// printf("TRY START %i\n", THIS_FRAME.try_info_ptr);
							// TODO: statical analysis and compile-time error when "try"s are nested more than MAX_TRIES_PER_FRAME
							assert(THIS_FRAME.try_info_ptr < MAX_TRIES_PER_FRAME);
							ARG(jo, JUMP_OFFSET);
							THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].catch_ip = ip + jo;
							THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].saved_stack_ptr = ctx->stack_ptr;
							THIS_FRAME.try_info_ptr++;
							// printf("TRY START %i\n", THIS_FRAME.try_info_ptr);
							goto main_loop;
		case OP_TRY_END:
							assert(THIS_FRAME.try_info_ptr);
							THIS_FRAME.try_info_ptr--;
							// printf("TRY END %i\n", THIS_FRAME.try_info_ptr);
							goto do_jump;
		case OP_ARR_REVERSE:
							EXPECT_STACK_DEPTH(1);
							array_reverse(TOP);
							goto main_loop;
		case OP_THROW:
							POP(*result);
							// if(obj_is_of_type(*result, vm->Exception)) {
							// 	set_normal_type_instance_attribute(*result, make_string("thrown_backtrace?"), make_backtrace(vm, ctx));
							// }
							goto exception;
		case OP_MAKE_CMD:
							EXPECT_STACK_DEPTH(2);
							command = make_normal_type_instance(vm->Command);
							POP_NOCHECK(v);
							set_normal_type_instance_attribute(command, make_string("redirects"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_attribute(command, make_string("argv"), v);
							PUSH_NOCHECK(command);
							goto main_loop;
		case OP_SET_CLOSURE_NAME:
							EXPECT_STACK_DEPTH(1);
							assert(IS_CLOSURE(TOP));
							v = make_string_of_len(&vm->bytecode[ip+1], vm->bytecode[ip]);
							ip += 1 + vm->bytecode[ip];
							// CLOSURE_OBJ_NAME(TOP) = v;
							if(!IS_HASH(CLOSURE_OBJ_ATTRS(TOP))) {
								goto main_loop;
							}
							set_hash_key(CLOSURE_OBJ_ATTRS(TOP), make_string("name"), v);
							goto main_loop;
		case OP_SET_CLOSURE_DOC:
							EXPECT_STACK_DEPTH(2);
							assert(IS_CLOSURE(SECOND));
							if(!IS_HASH(CLOSURE_OBJ_ATTRS(SECOND))) {
								goto main_loop;
							}
							set_hash_key(CLOSURE_OBJ_ATTRS(SECOND), make_string("doc"), FIRST);
							REMOVE_TOP_NOCHECK;
							goto main_loop;
		case OP_HASH_SET:
							EXPECT_STACK_DEPTH(3);
							set_hash_key(THIRD, SECOND, FIRST);
							REMOVE_TOP_N_NOCHECK(2);
							goto main_loop;
		case OP_HASH_UPDATE:
							EXPECT_STACK_DEPTH(2);
							update_hash(SECOND, FIRST);
							REMOVE_TOP_NOCHECK;
							goto main_loop;
		case OP_PUSH_KWARGS_MARKER:
							PUSH(MAKE_KWARGS_MARKER);
							goto main_loop;
		case OP_MAKE_REDIR:
							EXPECT_STACK_DEPTH(2);
							command = make_normal_type_instance(vm->Redir);
							POP_NOCHECK(v);
							set_normal_type_instance_attribute(command, make_string("datum"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_attribute(command, make_string("marker"), v);
							PUSH_NOCHECK(command);
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
							assert(0 == "Unknown opcode");
	}

end_main_loop:
	return METHOD_OK;

exception:
	// TOP is the excepion value
	if (THIS_FRAME.try_info_ptr) {
		// We have local exception handler
		THIS_FRAME.try_info_ptr--;
		ip = THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].catch_ip;
		ctx->stack_ptr = THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].saved_stack_ptr;
		PUSH(*result);
		goto main_loop;
	}

exception_return:
	// We don't handle the exception
	ctx->stack_ptr = saved_stack_ptr;
	return METHOD_EXCEPTION;
}
#undef GLOBALS
#undef LOCALS

// For composing bytecode
BYTECODE_HANDLE *ngs_create_bytecode() {
	BYTECODE_HANDLE *h;
	BYTECODE_SECTION_LEN len;
	char *p;
	h = NGS_MALLOC(sizeof(*h));
	assert(h);
	len = strlen(BYTECODE_SIGNATURE) + sizeof(BYTECODE_ORDER_CHECK) + sizeof(BYTECODE_SECTIONS_COUNT);
	h->data = NGS_MALLOC(len);
	h->len = len;
	p = h->data;

	memcpy(p, BYTECODE_SIGNATURE, strlen(BYTECODE_SIGNATURE));
	p += strlen(BYTECODE_SIGNATURE);

	BYTECODE_ADD(p, BYTECODE_ORDER_CHECK, BYTECODE_ORDER_CHECK_VAL);
	BYTECODE_ADD(p, BYTECODE_SECTIONS_COUNT, 0);

	return h;
}

// For composing bytecode
void ngs_add_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE type, BYTECODE_SECTION_LEN len, char *data) {
	char *p;
	size_t alloc_incr;
	alloc_incr = sizeof(BYTECODE_SECTION_TYPE) + sizeof(BYTECODE_SECTION_LEN) + len;
	h->data = NGS_REALLOC(h->data, h->len + alloc_incr);
	assert(h->data);
	p = &h->data[h->len];
	h->len += alloc_incr;

	BYTECODE_ADD(p, BYTECODE_SECTION_TYPE, type);
	BYTECODE_ADD(p, BYTECODE_SECTION_LEN, len);
	memcpy(p, data, len);

	p = &h->data[strlen(BYTECODE_SIGNATURE) + sizeof(BYTECODE_ORDER_CHECK)];
	(*(BYTECODE_SECTIONS_COUNT *) p)++;
}

BYTECODE_HANDLE *ngs_start_unserializing_bytecode(char *data) {
	BYTECODE_HANDLE *h;
	char *p;
	h = NGS_MALLOC(sizeof(*h));
	assert(h);
	p = data;
	h->data = data;
	h->next_section_num = 0;

	if(memcmp(p, BYTECODE_SIGNATURE, strlen(BYTECODE_SIGNATURE))) {
		assert(0 == "Bytecode has invalid signature");
	}
	p += strlen(BYTECODE_SIGNATURE);

	if(*(BYTECODE_ORDER_CHECK *)p != BYTECODE_ORDER_CHECK_VAL) {
		assert(0 == "Bytecode has invalid byte order");
	}
	p += sizeof(BYTECODE_ORDER_CHECK);

	h->sections_count = *(BYTECODE_SECTIONS_COUNT *)p;
	p += sizeof(BYTECODE_SECTIONS_COUNT);

	h->next_section_ptr = p;

	return h;
}

void ngs_fetch_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE *type, BYTECODE_SECTION_LEN *len, char **data) {
	char *p;
	if(h->next_section_num >= h->sections_count) {
		*type = 0;
		*len = 0;
		*data = NULL;
		return;
	}
	p = h->next_section_ptr;
	*type = *(BYTECODE_SECTION_TYPE *) p;
	p += sizeof(BYTECODE_SECTION_TYPE);
	*len = *(BYTECODE_SECTION_LEN *) p;
	p += sizeof(BYTECODE_SECTION_LEN);
	*data = p;
	p += *len;

	h->next_section_ptr = p;
	h->next_section_num++;
}

#undef BYTECODE_ADD
#undef BYTECODE_GET
