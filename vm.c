#include <assert.h>
#include <dlfcn.h>
#include <inttypes.h>
#include <math.h>
#include <pthread.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>

#ifdef HAVE_POLL_H
#include <sys/poll.h>
#endif

// REALPATH(3)
#include <limits.h>
#include <stdlib.h>

// GETTIMEOFDAY(2)
#include <sys/time.h>

#include <ffi.h>

// ..., FMEMOPEN(3)
#include <stdio.h>

#ifdef __APPLE__
#include "fmemopen.h"
#endif

// OPEN(2), LSEEK(2), WAIT(2)
// #define _POSIX_SOURCE
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <signal.h>
// READ(2), LSEEK(2), FORK(2), EXECVE(2), DUP2(2), SYSCONF(3)
#include <unistd.h>
// SOCKET(2), ...
#include <sys/socket.h>

// DIR
#include <dirent.h>

#include <errno.h>

#include <pcre.h>

#include "ngs.h"
#include "vm.h"
#include "ast.h"
#include "compile.h"
#include "syntax.h"
#include "syntax.auto.h"

extern char **environ;

// in syntax.include
void position_to_line_col(yycontext *yy, int pos, int result[]);


// in obj.c:
VALUE value_type(VM *vm, VALUE val);

char BYTECODE_SIGNATURE[] = "NGS BYTECODE";

char *opcodes_names[] = {
	"HALT",
	"PUSH_NULL",
	"PUSH_FALSE",
	"PUSH_TRUE",
	"PUSH_INT32",
	"PUSH_INT64",
	"PUSH_REAL",
	"PUSH_L8_STR",
	"PUSH_L32_STR",
	"DUP",
	"POP",
	"XCHG",
	"RESOLVE_GLOBAL",
	"PATCH",
	"FETCH_GLOBAL",
	"STORE_GLOBAL",
	"FETCH_LOCAL",
	"STORE_LOCAL",
	"CALL",
	"CALL_EXC",
	"CALL_ARR",
	"RET",
	"JMP",
	"JMP_TRUE",
	"JMP_FALSE",
	"MAKE_ARR",
	"MAKE_CLOSURE",
	"TO_STR",
	"MAKE_STR",
	"MAKE_STR_IMM",
	"MAKE_STR_EXP",
	"MAKE_STR_SPLAT_EXP",
	"PUSH_EMPTY_STR",
	"DEF_GLOBAL_FUNC",
	"DEF_LOCAL_FUNC",
	"FETCH_UPVAR",
	"STORE_UPVAR",
	"DEF_UPVAR_FUNC",
	"MAKE_HASH",
	"TO_BOOL",
	"TO_ARR",
	"TO_HASH",
	"ARR_APPEND",
	"ARR_APPEND2",
	"ARR_CONCAT",
	"GUARD",
	"TRY_START",
	"TRY_END",
	"ARR_REVERSE",
	"THROW",
	"MAKE_CMDS_PIPELINE",
	"MAKE_CMDS_PIPE",
	"MAKE_CMD",
	"SET_CLOSURE_NAME",
	"SET_CLOSURE_DOC",
	"SET_CLOSURE_NS",
	"HASH_SET",
	"HASH_UPDATE",
	"PUSH_KWARGS_MARKER",
	"MAKE_REDIR",
	"SUPER",
	"MAKE_MULTIMETHOD",
	"MULTIMETHOD_APPEND",
	"MULTIMETHOD_REVERSE",
};

#define ASSERT_STACK_SPACE if(ctx->stack_ptr>=MAX_STACK) goto stack_overflow_exception;
#define EXPECT_STACK_DEPTH(n) assert(ctx->stack_ptr >= (n));
#define PUSH(v) ASSERT_STACK_SPACE; ctx->stack[ctx->stack_ptr++] = v
#define PUSH_NOCHECK(v) ctx->stack[ctx->stack_ptr++] = v
#define POP(dst) assert(ctx->stack_ptr); ctx->stack_ptr--; (dst) = ctx->stack[ctx->stack_ptr]
#define POP_NOCHECK(dst) ctx->stack_ptr--; (dst) = ctx->stack[ctx->stack_ptr]
#define FIRST (ctx->stack[ctx->stack_ptr-1])
#define TOP FIRST
#define SECOND (ctx->stack[ctx->stack_ptr-2])
#define THIRD (ctx->stack[ctx->stack_ptr-3])
#define DUP ASSERT_STACK_SPACE; ctx->stack[ctx->stack_ptr] = ctx->stack[ctx->stack_ptr-1]; ctx->stack_ptr++;
#define REMOVE_TOP assert(ctx->stack_ptr); ctx->stack_ptr--
#define REMOVE_TOP_NOCHECK ctx->stack_ptr--
#define REMOVE_TOP_N(n) DEBUG_VM_RUN("Popping %d argument(s) after call from stack\n", (int)(n)); assert(ctx->stack_ptr >= (unsigned int)(n)); ctx->stack_ptr-=(n);
#define REMOVE_TOP_N_NOCHECK(n) DEBUG_VM_RUN("Popping %d argument(s) after call from stack\n", (int)(n)); ctx->stack_ptr-=(n);
#define GLOBALS (vm->globals)
#define THIS_FRAME (ctx->frames[ctx->frame_ptr-1])
#define THIS_FRAME_CLOSURE (THIS_FRAME.closure)
#define DEEPER_FRAME (ctx->frames[ctx->frame_ptr])
#define LOCALS (THIS_FRAME.locals)
#define UPLEVELS CLOSURE_OBJ_UPLEVELS(THIS_FRAME_CLOSURE)
#define ARG(name, type) name = *(type *) &vm->bytecode[ip]; ip += sizeof(type);
#define ARG_LVI ARG(lvi, LOCAL_VAR_INDEX);
#define ARG_GVI ARG(gvi, GLOBAL_VAR_INDEX);
#define ARG_UVI ARG(uvi, UPVAR_INDEX);

#define PUSH_METHOD_EXC(msg, dst, fn) \
	VALUE e; \
	e = make_normal_type_instance(vm->InvalidArgument); \
	set_normal_type_instance_field(e, make_string("message"), make_string(msg)); \
	set_normal_type_instance_field(e, make_string("backtrace"), make_backtrace(vm, ctx)); \
	set_normal_type_instance_field(e, make_string("target"), dst); \
	set_normal_type_instance_field(e, make_string("method"), fn); \
	*result = e; \
	goto exception;

#define PUSH_METHOD(dst, fn) \
	if(IS_NGS_TYPE(dst)) { \
		if(!IS_MULMETHOD(NGS_TYPE_CONSTRUCTORS(dst))) { \
			PUSH_METHOD_EXC("Can not push method as type constructor is not a MultiMethod", dst, fn); \
		} \
		push_multimethod_method(NGS_TYPE_CONSTRUCTORS(dst), (fn)); \
	} else { \
		if(!IS_MULMETHOD(dst)) { \
			PUSH_METHOD_EXC("Can not push method into non-MultiMethod. You are defining a method but the variable already exists and it is not a MultiMethod.", dst, fn); \
		} \
		push_multimethod_method(dst, fn); \
	};
// TODO: Exception
#define CONVERTING_OP(test, type) \
	assert(ctx->stack_ptr); \
	v = TOP; \
	if(test(v)) { \
		goto main_loop; \
	} \
	PUSH(v); \
	THIS_FRAME.last_ip = ip; \
	mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-2], vm->type, 1, &ctx->stack[ctx->stack_ptr-1]); \
	if(mr == METHOD_EXCEPTION) { \
		*result = ctx->stack[ctx->stack_ptr-2]; \
		goto exception; \
	} \
	if(mr != METHOD_OK) { \
		dump_titled(stderr, "Failed to convert to type", vm->type); \
		dump_titled(stderr, "Failed to convert value", ctx->stack[ctx->stack_ptr-1]); \
		assert(0 == "Failed to convert"); \
	} \
	REMOVE_TOP_N(1); \
	goto main_loop;

#define METHOD_PARAMS (VALUE *argv, VALUE *result)
#define EXT_METHOD_PARAMS (VM *vm, CTX *ctx, VALUE *argv, VALUE *result)
#define METHOD_RETURN(v) { *result = (v); return METHOD_OK; }
#define THROW_EXCEPTION(t) { *result = make_string(t); return METHOD_EXCEPTION; }
#define THROW_EXCEPTION_INSTANCE(e) { \
	set_normal_type_instance_field(e, make_string("backtrace"), make_backtrace(vm, ctx)); \
	*result = e; \
	return METHOD_EXCEPTION; \
}

#define INT_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _int_int METHOD_PARAMS { \
	SET_INT(*result, GET_INT(argv[0]) op GET_INT(argv[1])); \
	return METHOD_OK; \
}

#define INT_DIV_METHOD(name, op) \
METHOD_RESULT native_ ## name ## _int_int EXT_METHOD_PARAMS { \
	if(GET_INT(argv[1]) == 0) { \
		VALUE exc; \
		exc = make_normal_type_instance(vm->DivisionByZero); \
		THROW_EXCEPTION_INSTANCE(exc); \
	} \
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
	*(type *) (ptr) = val; \
	(ptr) += sizeof(type);

#define BYTECODE_GET(dst, ptr, type) \
	dst = *(type *) (ptr); \
	(ptr) += sizeof(type);

INT_METHOD(plus, +)
INT_METHOD(minus, -)
INT_METHOD(mul, *)
INT_METHOD(band, &)
INT_METHOD(bor, |)
INT_METHOD(bxor, ^)
INT_DIV_METHOD(div, /)
INT_DIV_METHOD(mod, %)
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
	dump(stderr, argv[0]);
	*result = MAKE_NULL;
	return METHOD_OK;
}

METHOD_RESULT native_false METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(MAKE_FALSE);
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
		e = make_normal_type_instance(vm->EmptyArrayFail);
		set_normal_type_instance_field(e, make_string("message"), make_string("pop(arr:Arr) failed because of empty array"));
		THROW_EXCEPTION_INSTANCE(e);
	}
	*result = ARRAY_ITEMS(argv[0])[OBJ_LEN(argv[0])-1];
	OBJ_LEN(argv[0])--;
	return METHOD_OK;
}

METHOD_RESULT native_shift_arr EXT_METHOD_PARAMS {
	if(!OBJ_LEN(argv[0])) {
		VALUE e;
		e = make_normal_type_instance(vm->EmptyArrayFail);
		set_normal_type_instance_field(e, make_string("message"), make_string("shift(arr:Arr) failed because of empty array"));
		THROW_EXCEPTION_INSTANCE(e);
	}
	METHOD_RETURN(array_shift(argv[0]));
}

METHOD_RESULT native_shift_arr_any METHOD_PARAMS {
	if(!OBJ_LEN(argv[0])) {
		METHOD_RETURN(argv[1]);
	}
	METHOD_RETURN(array_shift(argv[0]));
}

METHOD_RESULT native_index_get_arr_int EXT_METHOD_PARAMS {
	int idx, len;
	(void) ctx;
	len = OBJ_LEN(argv[0]);
	idx = GET_INT(argv[1]);
	if((idx < 0) || (idx >= len)) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_field(e, make_string("container"), argv[0]);
		set_normal_type_instance_field(e, make_string("key"), argv[1]);
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
		set_normal_type_instance_field(e, make_string("container"), argv[0]);
		set_normal_type_instance_field(e, make_string("key"), argv[1]);
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

METHOD_RESULT native_round_real METHOD_PARAMS { METHOD_RETURN(make_real((NGS_REAL) round(GET_REAL(argv[0])))); }
METHOD_RESULT native_trunc_real METHOD_PARAMS { METHOD_RETURN(make_real((NGS_REAL) trunc(GET_REAL(argv[0])))); }
METHOD_RESULT native_floor_real METHOD_PARAMS { METHOD_RETURN(make_real((NGS_REAL) floor(GET_REAL(argv[0])))); }
METHOD_RESULT native_ceil_real METHOD_PARAMS { METHOD_RETURN(make_real((NGS_REAL) ceil(GET_REAL(argv[0])))); }
METHOD_RESULT native_c_pow_real_real EXT_METHOD_PARAMS {
	VALUE ret;
	int status = 0;
	NGS_REAL t = pow(GET_REAL(argv[0]), GET_REAL(argv[1]));
	ret = make_array(2);

	if(t == NGS_REAL_HUGE_VAL) {
		status = 1;
	} else if(t == -NGS_REAL_HUGE_VAL) {
		status = -1;
	}
	ARRAY_ITEMS(ret)[0] = MAKE_INT(status);
	ARRAY_ITEMS(ret)[1] = make_real((NGS_REAL) t);
	METHOD_RETURN(ret);
	// ret = make_array_with_values(2, (VALUES *){MAKE_BOOL(ret != HUGE_VAL && ret != -HUGE_VAL), ((NGS_REAL) ret)});
}

// TODO: Handle precision issue around INT_MAX input. Probably around INT_MIN too.
//       On my system INT_MAX is 2305843009213693951 and when converted to NGS_REAL becomes 2305843009213693952.0
//       "r > (NGS_REAL) NGS_INT_MAX" is false for the numbers below so the if is not entered.
//       "Int(2305843009213693952.0)" is -2305843009213693952
//       "Int(2305843009213694208.0)" is -2305843009213693952
//       Also:
//       "Real(INT_MAX) + 1.0 > Real(INT_MAX)" is false
//       "Int(Real(INT_MAX))" is -2305843009213693952
//       "Int(Real(INT_MAX)) == INT_MIN" is true
METHOD_RESULT native_Int_real EXT_METHOD_PARAMS {
	NGS_REAL r = GET_REAL(argv[0]);
	// printf("%d\n", 2305843009213694208.0 > 2305843009213693952.0); // 0
	if((r > (NGS_REAL) NGS_INT_MAX) || (r < (NGS_REAL) NGS_INT_MIN)) {
		VALUE e;
        e = make_normal_type_instance(vm->InvalidArgument);
        set_normal_type_instance_field(e, make_string("message"), make_string("Could not convert given float to integer: out of supported range."));
        set_normal_type_instance_field(e, make_string("value"), argv[0]);
        set_normal_type_instance_field(e, make_string("min"), make_real((NGS_REAL) NGS_INT_MIN));
        set_normal_type_instance_field(e, make_string("max"), make_real((NGS_REAL) NGS_INT_MAX));
		// printf("%ld %f\n", NGS_INT_MAX, (NGS_REAL) (NGS_INT_MAX));
        THROW_EXCEPTION_INSTANCE(e);
	}
	METHOD_RETURN(MAKE_INT((intptr_t) r));
}

METHOD_RESULT native_Int_str_int EXT_METHOD_PARAMS {
    char *nptr, *endptr;
    long long r;
    nptr = obj_to_cstring(argv[0]);
    // https://en.cppreference.com/w/c/string/byte/strtol
    r = strtoll(nptr, &endptr, GET_INT(argv[1]));
    if (
            nptr == endptr ||
            endptr - nptr < OBJ_LEN(argv[0]) ||
            ((r == LONG_MAX || r == LONG_MIN || r == LLONG_MAX || r == LLONG_MIN) && errno == ERANGE)) {
        VALUE e;
        e = make_normal_type_instance(vm->InvalidArgument);
        set_normal_type_instance_field(e, make_string("message"), make_string("Could not convert given string to integer."));
        THROW_EXCEPTION_INSTANCE(e);
    }
    METHOD_RETURN(MAKE_INT(r));
}

METHOD_RESULT native_is_any_type EXT_METHOD_PARAMS {
	(void) ctx;
	SET_BOOL(*result, obj_is_of_type(vm, argv[0], argv[1]));
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
		set_normal_type_instance_field(exc, make_string("container"), argv[0]);
		set_normal_type_instance_field(exc, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	METHOD_RETURN(e->val)
}

METHOD_RESULT native_index_set_hash_any_any METHOD_PARAMS { set_hash_key(argv[0], argv[1], argv[2]); METHOD_RETURN(argv[2]); }

METHOD_RESULT native_index_del_hash_any EXT_METHOD_PARAMS {
	if(!del_hash_key(argv[0], argv[1])) {
		VALUE exc;
		exc = make_normal_type_instance(vm->KeyNotFound);
		set_normal_type_instance_field(exc, make_string("container"), argv[0]);
		set_normal_type_instance_field(exc, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	METHOD_RETURN(argv[0])
}

// TODO: factor out common in native_ll_hash_{head,tail}
METHOD_RESULT native_ll_hash_head EXT_METHOD_PARAMS {
	VALUE ret;
	HASH_OBJECT_ENTRY *e = HASH_HEAD(argv[0]);
	if(!e) {
		METHOD_RETURN(MAKE_NULL);
	};
	HASH_ENTRY_OBJECT *heo;
	heo = NGS_MALLOC(sizeof(*heo));
	assert(heo);
	heo->entry = e;
	SET_OBJ(ret, heo);
	OBJ_TYPE_NUM(ret) = T_LL_HASH_ENTRY;
	METHOD_RETURN(ret);
}

METHOD_RESULT native_ll_hash_tail EXT_METHOD_PARAMS {
	VALUE ret;
	HASH_OBJECT_ENTRY *e = HASH_TAIL(argv[0]);
	if(!e) {
		METHOD_RETURN(MAKE_NULL);
	};
	HASH_ENTRY_OBJECT *heo;
	heo = NGS_MALLOC(sizeof(*heo));
	assert(heo);
	heo->entry = e;
	SET_OBJ(ret, heo);
	OBJ_TYPE_NUM(ret) = T_LL_HASH_ENTRY;
	METHOD_RETURN(ret);
}

METHOD_RESULT native_ll_hash_entry_key EXT_METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e = ((HASH_ENTRY_OBJECT *)(argv[0].ptr))->entry;
	METHOD_RETURN(e->key);
}

METHOD_RESULT native_ll_hash_entry_val EXT_METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e = ((HASH_ENTRY_OBJECT *)(argv[0].ptr))->entry;
	METHOD_RETURN(e->val);
}

METHOD_RESULT native_ll_hash_entry_next EXT_METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e = ((HASH_ENTRY_OBJECT *)(argv[0].ptr))->entry;
	if(!e->insertion_order_next) {
		METHOD_RETURN(MAKE_NULL);
	};

	VALUE ret;
	HASH_ENTRY_OBJECT *heo;
	heo = NGS_MALLOC(sizeof(*heo));
	assert(heo);
	heo->entry = e->insertion_order_next;
	SET_OBJ(ret, heo);
	OBJ_TYPE_NUM(ret) = T_LL_HASH_ENTRY;
	METHOD_RETURN(ret);
}

METHOD_RESULT native_Hash_nti METHOD_PARAMS {
	VALUE ut, item;
	HASH_OBJECT_ENTRY *e;

	ut = NORMAL_TYPE_INSTANCE_TYPE(argv[0]);
	// TODO: round to nearest power of 2?
	*result = make_hash(OBJ_LEN(NGS_TYPE_FIELDS(ut)));
	for(e=HASH_HEAD(NGS_TYPE_FIELDS(ut)); e; e=e->insertion_order_next) {
		if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(argv[0])) <= (size_t)GET_INT(e->val)) {
			continue;
		}
		item = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[0]))[(size_t)GET_INT(e->val)];
		if(!IS_UNDEF(item)) {
			set_hash_key(*result, e->key, item);
		}
	}
	return METHOD_OK;
}

METHOD_RESULT native_Namespace METHOD_PARAMS {
	*result = make_namespace(0);
	return METHOD_OK;
};

// TODO: locking for dlerror?
METHOD_RESULT native_c_dlopen_str_int EXT_METHOD_PARAMS {
	VALUE v;
	CLIB_OBJECT *o;
	void *out;
	out = dlopen(obj_to_cstring(argv[0]), GET_INT(argv[1]));
	if(!out) {
		VALUE e;
		e = make_normal_type_instance(vm->DlopenFail);
		// XXX: Could not find whether we should free the (char *) returned by dlerror()
		set_normal_type_instance_field(e, make_string("message"), make_string(dlerror()));
		set_normal_type_instance_field(e, make_string("filename"), argv[0]);
		THROW_EXCEPTION_INSTANCE(e);
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

METHOD_RESULT native_index_get_clib_str EXT_METHOD_PARAMS {
	VALUE v;
	CSYM_OBJECT *o;
	o = NGS_MALLOC(sizeof(*o));
	assert(o);
	o->base.type.num = T_CSYM;
	o->base.val.ptr = dlsym(OBJ_DATA_PTR(argv[0]), obj_to_cstring(argv[1]));
	if(!o->base.val.ptr) {
		VALUE e;
		e = make_normal_type_instance(vm->Error);
		set_normal_type_instance_field(e, make_string("message"), make_string("Failed to dlsym()"));
		set_normal_type_instance_field(e, make_string("handle"), argv[0]);
		set_normal_type_instance_field(e, make_string("symbol"), argv[1]);
		THROW_EXCEPTION_INSTANCE(e);
	}
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

METHOD_RESULT native_c_realpath_str METHOD_PARAMS {
	const char *path = obj_to_cstring(argv[0]);
	char resolved_path[PATH_MAX + 1];
	resolved_path[0] = '\0';
	if(realpath(path, resolved_path)) {
		*result = make_string(resolved_path);
	} else {
		*result = MAKE_NULL;
	}
	METHOD_RETURN(*result);
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
				set_normal_type_instance_field(exc, make_string("which"), make_string("Third parameter to c_lseek(), 'whence'"));
				set_normal_type_instance_field(exc, make_string("given"), argv[2]);
				// TODO: Array of expected values maybe?
				set_normal_type_instance_field(exc, make_string("expected"), make_string("One of: 'set', 'cur', 'end'"));
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
	METHOD_RETURN(MAKE_BOOL(!memcmp(OBJ_DATA_PTR(argv[0]), OBJ_DATA_PTR(argv[1]), len)));
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

// TODO: make "start" and "end" size_t to simplify the code
// TODO: better than METHOD_ARGS_MISMATCH on include_start != true and include_end != false
#define NATIVE_RANGE_INDEX_SETUP \
	if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(argv[1])) < 5) return METHOD_ARGS_MISMATCH; \
	include_start = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[RANGE_FIELD_INCLUDE_START]; \
	if(!IS_BOOL(include_start)) return METHOD_ARGS_MISMATCH; \
	if(!IS_TRUE(include_start)) return METHOD_ARGS_MISMATCH; \
	include_end = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[RANGE_FIELD_INCLUDE_END]; \
	if(!IS_BOOL(include_end)) return METHOD_ARGS_MISMATCH; \
	if(!IS_FALSE(include_end)) return METHOD_ARGS_MISMATCH; \
	start = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[RANGE_FIELD_START]; \
	if(!IS_INT(start)) return METHOD_ARGS_MISMATCH; \
	if(GET_INT(start) < 0) { \
		exc = make_normal_type_instance(vm->IndexNotFound); \
		set_normal_type_instance_field(exc, make_string("message"), make_string("Negative range start")); \
		THROW_EXCEPTION_INSTANCE(exc); \
	} \
	if(((size_t) GET_INT(start)) > OBJ_LEN(argv[0])) { \
		exc = make_normal_type_instance(vm->IndexNotFound); \
		set_normal_type_instance_field(exc, make_string("message"), make_string("NumRange starts after string/array end")); \
		THROW_EXCEPTION_INSTANCE(exc); \
	} \
	end = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(argv[1]))[RANGE_FIELD_END]; \
	if(IS_NULL(end)) { \
		end = MAKE_INT(OBJ_LEN(argv[0])); \
	} \
	if(!IS_INT(end)) return METHOD_ARGS_MISMATCH; \
	if(GET_INT(end) < GET_INT(start)) { \
		exc = make_normal_type_instance(vm->InvalidArgument); \
		set_normal_type_instance_field(exc, make_string("message"), make_string("NumRange end smaller than range start when calling [](s:Str, r:NumRange)")); \
		THROW_EXCEPTION_INSTANCE(exc); \
	} \
	len = GET_INT(end) - GET_INT(start); \
	if(GET_INT(start) + len > OBJ_LEN(argv[0])) { \
		exc = make_normal_type_instance(vm->IndexNotFound); \
		set_normal_type_instance_field(exc, make_string("message"), make_string("NumRange ends after string end")); \
		THROW_EXCEPTION_INSTANCE(exc); \
	}

METHOD_RESULT native_index_get_str_range EXT_METHOD_PARAMS {
	size_t len;
	VALUE start, end, include_start, include_end, exc;
	(void) ctx;
	NATIVE_RANGE_INDEX_SETUP;
	*result = make_string_of_len(NULL, len);
	memcpy(OBJ_DATA_PTR(*result), OBJ_DATA_PTR(argv[0]) + GET_INT(start), len);
	return METHOD_OK;
}

METHOD_RESULT native_index_set_str_range_str EXT_METHOD_PARAMS {
	size_t len, new_total_len;
	VALUE start, end, include_start, include_end, exc;
	char *old_data;
	(void) ctx;
	NATIVE_RANGE_INDEX_SETUP;
	new_total_len = GET_INT(start) + OBJ_LEN(argv[2]) + (OBJ_LEN(argv[0]) - GET_INT(end));
	old_data = OBJ_DATA_PTR(argv[0]);
	OBJ_DATA_PTR(argv[0]) = NGS_MALLOC_ATOMIC(new_total_len);
	memcpy(OBJ_DATA_PTR(argv[0]), old_data, GET_INT(start));
	if(OBJ_LEN(argv[2])) {
		memcpy(OBJ_DATA_PTR(argv[0]) + GET_INT(start), OBJ_DATA_PTR(argv[2]), OBJ_LEN(argv[2]));
	}
	memcpy(OBJ_DATA_PTR(argv[0]) + GET_INT(start) + OBJ_LEN(argv[2]), old_data + GET_INT(end), OBJ_LEN(argv[0]) - GET_INT(end));
	OBJ_LEN(argv[0]) = new_total_len;
	METHOD_RETURN(argv[2]);
}

// TODO: maybe use vlo->item_size and unify Str and Arr methods
METHOD_RESULT native_index_get_arr_range EXT_METHOD_PARAMS {
	size_t len;
	VALUE start, end, include_start, include_end, exc;
	(void) ctx;
	NATIVE_RANGE_INDEX_SETUP;
	*result = make_array(len);
	memcpy(OBJ_DATA_PTR(*result), &ARRAY_ITEMS(argv[0])[GET_INT(start)], len*sizeof(VALUE));
	return METHOD_OK;
}

// TODO: maybe use vlo->item_size and unify Str and Arr methods
METHOD_RESULT native_index_set_arr_range_arr EXT_METHOD_PARAMS {
	size_t len, new_total_len;
	VALUE start, end, include_start, include_end, exc;
	char *old_data;
	(void) ctx;
	NATIVE_RANGE_INDEX_SETUP;
	new_total_len = GET_INT(start) + OBJ_LEN(argv[2]) + (OBJ_LEN(argv[0]) - GET_INT(end));
	old_data = OBJ_DATA_PTR(argv[0]);
	OBJ_DATA_PTR(argv[0]) = NGS_MALLOC(new_total_len * sizeof(VALUE));
	memcpy(&ARRAY_ITEMS(argv[0])[0], old_data, GET_INT(start) * sizeof(VALUE));
	if(OBJ_LEN(argv[2])) {
		memcpy(&ARRAY_ITEMS(argv[0])[GET_INT(start)], OBJ_DATA_PTR(argv[2]), OBJ_LEN(argv[2]) * sizeof(VALUE));
	}
	memcpy(&ARRAY_ITEMS(argv[0])[GET_INT(start) + OBJ_LEN(argv[2])], old_data + GET_INT(end) * sizeof(VALUE), (OBJ_LEN(argv[0]) - GET_INT(end)) * sizeof(VALUE));
	OBJ_LEN(argv[0]) = new_total_len;
	METHOD_RETURN(argv[2]);
}

METHOD_RESULT native_eq_bool_bool METHOD_PARAMS { METHOD_RETURN(MAKE_BOOL(argv[0].num == argv[1].num)); }
METHOD_RESULT native_not_bool METHOD_PARAMS { METHOD_RETURN(MAKE_BOOL(argv[0].num == V_FALSE)); }

// XXX: glibc specific fmemopen()
METHOD_RESULT native_compile_str_str EXT_METHOD_PARAMS {
	ast_node *tree = NULL;
	yycontext yyctx;
	int parse_ok;
	(void) ctx;
	memset(&yyctx, 0, sizeof(yycontext));
	yyctx.fail_pos = -1;
	yyctx.fail_rule = "(unknown)";
	yyctx.lines = 0;
	yyctx.lines_postions[0] = 0;
	// printf("PT 1 %p\n", OBJ_DATA_PTR(argv[0]));

	if(OBJ_LEN(argv[0]) == 0) {
		VALUE exc;
		exc = make_normal_type_instance(vm->CompileFail);
		set_normal_type_instance_field(exc, make_string("message"), make_string("Empty string was passed as input to compile(). Trying to run an empty file?"));
		THROW_EXCEPTION_INSTANCE(exc);
	}

	yyctx.input_file = fmemopen(OBJ_DATA_PTR(argv[0]), OBJ_LEN(argv[0]), "r");
	parse_ok = yyparse(&yyctx);
	if(!parse_ok) {
		VALUE exc;
		char *err = NGS_MALLOC_ATOMIC(1024);
		exc = make_normal_type_instance(vm->CompileFail);
		set_normal_type_instance_field(exc, make_string("given"), argv[0]);

		int *pos_line_col = NGS_MALLOC_ATOMIC(sizeof(int) * 2);
		position_to_line_col(&yyctx, yyctx.fail_pos, pos_line_col);

		VALUE pos = make_hash(4);
		set_hash_key(pos, make_string("file"), argv[1]);
		set_hash_key(pos, make_string("absolute"), MAKE_INT(yyctx.fail_pos));
		set_hash_key(pos, make_string("line"), MAKE_INT(pos_line_col[0]));
		set_hash_key(pos, make_string("column"), MAKE_INT(pos_line_col[1]));

		set_normal_type_instance_field(exc, make_string("position"), pos);
		set_normal_type_instance_field(exc, make_string("rule"), make_string(yyctx.fail_rule));
		THROW_EXCEPTION_INSTANCE(exc);
	}
	tree = yyctx.__;
	IF_DEBUG(COMPILER, print_ast(tree, 0);)
	yyrelease(&yyctx);
	COMPILATION_RESULT *r = compile(tree, obj_to_cstring(argv[1]));
	VALUE ret = make_string_of_len(r->bytecode, r->len);
	OBJ_META(ret) = make_hash(1);
	set_hash_key(OBJ_META(ret), make_string("warnings"), r->warnings);
	METHOD_RETURN(ret);
}

METHOD_RESULT native_load_str_str EXT_METHOD_PARAMS {
	size_t ip;
	(void) ctx;
	ip = vm_load_bytecode(vm, OBJ_DATA_PTR(argv[0]));
	METHOD_RETURN(make_closure_obj(ip, 0, 0, 0, 0, 0, NULL, NULL));
}

METHOD_RESULT native_decode_json_str EXT_METHOD_PARAMS {
	METHOD_RESULT mr;
	(void) ctx;
	mr = decode_json(vm, argv[0], result);
	if(mr == METHOD_EXCEPTION) {
		set_normal_type_instance_field(*result, make_string("backtrace"), make_backtrace(vm, ctx));
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
		// could be big... // set_normal_type_instance_field(exc, make_string("data"), argv[0]);
		set_normal_type_instance_field(exc, make_string("message"), *result);
		set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
		*result = exc;
	}
	return mr;
}

METHOD_RESULT native_backtrace EXT_METHOD_PARAMS { (void) argv; METHOD_RETURN(make_backtrace(vm, ctx)); }
METHOD_RESULT native_resolve_instruction_pointer EXT_METHOD_PARAMS { (void) ctx; METHOD_RETURN(resolve_instruction_pointer(vm, GET_INT(argv[0]))); }

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

METHOD_RESULT native_c_time METHOD_PARAMS { (void) argv; METHOD_RETURN(MAKE_INT((long int)time(NULL))); }

METHOD_RESULT native_c_gettimeofday METHOD_PARAMS {
	struct timeval tv;
	gettimeofday(&tv, NULL);
	VALUE ret;
	ret = make_array(2);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(tv.tv_sec);
	ARRAY_ITEMS(ret)[1] = MAKE_INT(tv.tv_usec);
	METHOD_RETURN(ret);
}

#define ELT(value) *p = MAKE_INT(value); p++;
METHOD_RESULT native_c_gmtime EXT_METHOD_PARAMS {
		struct tm t;
		time_t arg = (time_t) GET_INT(argv[0]);
		VALUE ret, *p;
		(void) ctx;
		gmtime_r(&arg, &t);
		ret = make_normal_type_instance(vm->c_tm);
		OBJ_DATA(ret) = make_array(9);
		p = ARRAY_ITEMS(OBJ_DATA(ret));
		ELT(t.tm_sec); ELT(t.tm_min); ELT(t.tm_hour); ELT(t.tm_mday); ELT(t.tm_mon); ELT(t.tm_year); ELT(t.tm_wday); ELT(t.tm_yday); ELT(t.tm_isdst);
		METHOD_RETURN(ret);
}
METHOD_RESULT native_c_localtime EXT_METHOD_PARAMS {
		struct tm t;
		time_t arg = (time_t) GET_INT(argv[0]);
		VALUE ret, *p;
		(void) ctx;
		localtime_r(&arg, &t);
		ret = make_normal_type_instance(vm->c_tm);
		OBJ_DATA(ret) = make_array(9);
		p = ARRAY_ITEMS(OBJ_DATA(ret));
		ELT(t.tm_sec); ELT(t.tm_min); ELT(t.tm_hour); ELT(t.tm_mday); ELT(t.tm_mon); ELT(t.tm_year); ELT(t.tm_wday); ELT(t.tm_yday); ELT(t.tm_isdst);
		METHOD_RETURN(ret);
}
#undef ELT

METHOD_RESULT native_c_strftime METHOD_PARAMS {
	size_t size;
	struct tm t;
	char *s = NGS_MALLOC_ATOMIC(1024);
	assert(s);
	memset(&t, 0, sizeof(t));
	t.tm_sec   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[0]);
	t.tm_min   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[1]);
	t.tm_hour  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[2]);
	t.tm_mday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[3]);
	t.tm_mon   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[4]);
	t.tm_year  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[5]);
	t.tm_wday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[6]);
	t.tm_yday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[7]);
	t.tm_isdst = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[8]);
	size = strftime(s, 1024, obj_to_cstring(argv[1]), &t);
	if(size) {
		METHOD_RETURN(make_string_of_len(s, size));
	} else {
		METHOD_RETURN(MAKE_NULL);
	}
}

// TODO: make 9 a constant
METHOD_RESULT native_c_strptime EXT_METHOD_PARAMS {
	VALUE ret = make_array(2);
	struct tm t;
	memset(&t, '\0', sizeof(t));
	char *input = obj_to_cstring(argv[0]);
	char *next_char = strptime(input, obj_to_cstring(argv[1]), &t);
	if(next_char == NULL) {
		ARRAY_ITEMS(ret)[0] = MAKE_INT(0);
		ARRAY_ITEMS(ret)[1] = MAKE_NULL;
	} else {
		ARRAY_ITEMS(ret)[0] = MAKE_INT(next_char - input);
		VALUE ret_tm = make_normal_type_instance(vm->c_tm);
		OBJ_DATA(ret_tm) = make_array(9);
		VALUE *p = ARRAY_ITEMS(OBJ_DATA(ret_tm));
		*(p++) = MAKE_INT(t.tm_sec);
		*(p++) = MAKE_INT(t.tm_min);
		*(p++) = MAKE_INT(t.tm_hour);
		*(p++) = MAKE_INT(t.tm_mday);
		*(p++) = MAKE_INT(t.tm_mon);
		*(p++) = MAKE_INT(t.tm_year);
		*(p++) = MAKE_INT(t.tm_wday);
		*(p++) = MAKE_INT(t.tm_yday);
		*(p++) = MAKE_INT(t.tm_isdst);
		ARRAY_ITEMS(ret)[1] = ret_tm;
	}
	METHOD_RETURN(ret);
}

// TODO: factor out c_tm -> struct tm conversion
METHOD_RESULT native_c_mktime METHOD_PARAMS {
	struct tm t;
	memset(&t, 0, sizeof(t));
	t.tm_sec   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[0]);
	t.tm_min   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[1]);
	t.tm_hour  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[2]);
	t.tm_mday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[3]);
	t.tm_mon   = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[4]);
	t.tm_year  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[5]);
	t.tm_year  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[5]);
	t.tm_wday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[6]);
	t.tm_yday  = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[7]);
	t.tm_isdst = GET_INT(ARRAY_ITEMS(OBJ_DATA(argv[0]))[8]);
	time_t tt = mktime(&t);
	METHOD_RETURN(MAKE_INT(tt));
}

METHOD_RESULT native_type_str_doc_ns METHOD_PARAMS {
	*result = make_normal_type(argv[0]);
	set_hash_key(OBJ_META(*result), make_string("doc"), argv[1]);
	set_hash_key(OBJ_META(*result), make_string("ns"), argv[2]);
	return METHOD_OK;
}

METHOD_RESULT native_typeof_any EXT_METHOD_PARAMS {
	(void) ctx;
	METHOD_RETURN(value_type(vm, argv[0]));
}

METHOD_RESULT native_get_field_nti_str EXT_METHOD_PARAMS {
	// WARNING: for now get_normal_type_instance_field can only throw FieldNotFound
	//          if it changes in future the calling convention below should be changed
	//          The reason for such calling convention is not to pass the VM to
	//          get_normal_type_instance_field() just so it will have access to the
	//          exceptions.
	METHOD_RESULT mr;
	(void) ctx;
	mr = get_normal_type_instance_field(argv[0], argv[1], result);
	if(mr == METHOD_EXCEPTION) {
		VALUE exc;
		exc = make_normal_type_instance(vm->FieldNotFound);
		set_normal_type_instance_field(exc, make_string("container"), argv[0]);
		set_normal_type_instance_field(exc, make_string("key"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	return mr;
}
METHOD_RESULT native_set_field_nti_str_any METHOD_PARAMS { set_normal_type_instance_field(argv[0], argv[1], argv[2]); METHOD_RETURN(argv[2]); }

METHOD_RESULT native_in_nti_str METHOD_PARAMS {
	METHOD_RESULT mr;
	mr = get_normal_type_instance_field(argv[1], argv[0], result);
	METHOD_RETURN(MAKE_BOOL(mr != METHOD_EXCEPTION));
}

METHOD_RESULT native_inherit_t_t METHOD_PARAMS { add_type_inheritance(argv[0], argv[1]); METHOD_RETURN(argv[0]); }

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
			set_normal_type_instance_field(exc, make_string("message"), make_string("join - array must contain only strings"));
			set_normal_type_instance_field(exc, make_string("given"), ARRAY_ITEMS(argv[0])[i]);
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
	if(pid == 0) {
		// Child
		NGS_NOTIFY_MALLOC_ABOUT_FORK();
	}
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

// TODO: dedup native_get_field_bt_str / native_get_field_nt_str
METHOD_RESULT native_get_field_bt_str EXT_METHOD_PARAMS {
	VALUE exc;
	char *field = obj_to_cstring(argv[1]);
	(void) ctx;
	if(!strcmp(field, "constructors")) {
		METHOD_RETURN(NGS_TYPE_CONSTRUCTORS(argv[0]));
	}
	if(!strcmp(field, "name")) {
		METHOD_RETURN(NGS_TYPE_NAME(argv[0]));
	}
	if(!strcmp(field, "parents")) {
		METHOD_RETURN(NGS_TYPE_PARENTS(argv[0]));
	}

	exc = make_normal_type_instance(vm->FieldNotFound);
	set_normal_type_instance_field(exc, make_string("container"), argv[0]);
	set_normal_type_instance_field(exc, make_string("key"), argv[1]);
	THROW_EXCEPTION_INSTANCE(exc);
}

// TODO: Factor out "constructors", "name", ...
METHOD_RESULT native_get_field_nt_str EXT_METHOD_PARAMS {
	VALUE exc;
	char *field = obj_to_cstring(argv[1]);
	(void) ctx;
	if(!strcmp(field, "constructors")) {
		METHOD_RETURN(NGS_TYPE_CONSTRUCTORS(argv[0]));
	}
	if(!strcmp(field, "name")) {
		METHOD_RETURN(NGS_TYPE_NAME(argv[0]));
	}
	if(!strcmp(field, "parents")) {
		METHOD_RETURN(NGS_TYPE_PARENTS(argv[0]));
	}
	if(!strcmp(field, "user")) {
		METHOD_RETURN(NGS_TYPE_USER(argv[0]));
	}

	exc = make_normal_type_instance(vm->FieldNotFound);
	set_normal_type_instance_field(exc, make_string("container"), argv[0]);
	set_normal_type_instance_field(exc, make_string("key"), argv[1]);
	THROW_EXCEPTION_INSTANCE(exc);
}

METHOD_RESULT native_set_field_nt_str EXT_METHOD_PARAMS {
	VALUE exc;
	char *field = obj_to_cstring(argv[1]);
	(void) ctx;
	if(!strcmp(field, "user")) {
		NGS_TYPE_USER(argv[0]) = argv[2];
		METHOD_RETURN(argv[1]);
	}

	exc = make_normal_type_instance(vm->FieldNotFound);
	set_normal_type_instance_field(exc, make_string("container"), argv[0]);
	set_normal_type_instance_field(exc, make_string("key"), argv[1]);
	VALUE ak = make_array(1);
	ARRAY_ITEMS(ak)[0] = make_string("user");
	set_normal_type_instance_field(exc, make_string("available_keys"), ak);
	THROW_EXCEPTION_INSTANCE(exc);
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

METHOD_RESULT native_c_errno METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(MAKE_INT(errno));
}

METHOD_RESULT native_c_strerror METHOD_PARAMS {
	METHOD_RETURN(make_string(strerror(GET_INT(argv[0]))));
}

METHOD_RESULT native_c_execve METHOD_PARAMS {
	char *exec_filename;
	char **exec_argv, **exec_envp;
	exec_filename = obj_to_cstring(argv[0]);
	exec_argv = obj_to_cstring_array(argv[1]);
	exec_envp = obj_to_cstring_array(argv[2]);
	METHOD_RETURN(MAKE_INT(execve(exec_filename, exec_argv, exec_envp)));
}

METHOD_RESULT native_C_WEXITSTATUS METHOD_PARAMS {
	int status = (int) GET_INT(argv[0]);
	if(WIFEXITED(status)) {
		METHOD_RETURN(MAKE_INT(WEXITSTATUS(status)));
	} else {
		METHOD_RETURN(MAKE_NULL);
}
	}
METHOD_RESULT native_C_WTERMSIG METHOD_PARAMS {
	int status = (int) GET_INT(argv[0]);
	if(WIFSIGNALED(status)) {
		METHOD_RETURN(MAKE_INT(WTERMSIG(status)));
	} else {
		METHOD_RETURN(MAKE_NULL);
	}
}

GLOBAL_VAR_INDEX check_global_index(VM *vm, const char *name, size_t name_len, int *found) {
	HASH_OBJECT_ENTRY *e = get_hash_key(vm->globals_indexes, make_string_of_len(name, name_len));
	if(e) {
		*found = 1;
		return (GLOBAL_VAR_INDEX)GET_INT(e->val);
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

METHOD_RESULT native_meta METHOD_PARAMS {
	if(!IS_OBJ(argv[0])) {
		return METHOD_ARGS_MISMATCH;
	}
	METHOD_RETURN(OBJ_META(argv[0]));
}
METHOD_RESULT native_meta_any METHOD_PARAMS {
	if(!IS_OBJ(argv[0])) {
		return METHOD_ARGS_MISMATCH;
	}
	OBJ_META(argv[0]) = argv[1]; METHOD_RETURN(argv[1]);
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

METHOD_RESULT native_ip_closure METHOD_PARAMS { METHOD_RETURN(MAKE_INT(CLOSURE_OBJ_IP(argv[0]))); }

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

METHOD_RESULT native_c_strcmp METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(strcmp(obj_to_cstring(argv[0]), obj_to_cstring(argv[1]))));
}

METHOD_RESULT native_c_pthreadattrt METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(make_pthread_attr());
}

METHOD_RESULT native_c_pthreadmutext METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(make_pthread_mutex());
}

METHOD_RESULT native_c_pthreadmutexattrt METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(make_pthread_mutexattr());
}

METHOD_RESULT native_c_pthreadcondt METHOD_PARAMS {
	(void) argv;
	METHOD_RETURN(make_pthread_cond());
}

// https://linux.die.net/man/3/pthread_mutexattr_settype
METHOD_RESULT native_c_pthreadmutexattrsettype_pma_int METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(pthread_mutexattr_settype(&GET_PTHREADMUTEXATTR(argv[0]), GET_INT(argv[1]))));
}

// TODO: For gcc-5 and on consider: #define ATTR ((pthread_attr_t * restrict)&GET_PTHREADATTR(argv[0]))
#define ATTR ((pthread_attr_t *)&GET_PTHREADATTR(argv[0]))
// TODO: check range - i might be larger than supported MAKE_INT() argument
#define INT_ATTR(name) \
	if(!strcmp(field, #name)) { \
		pthread_attr_get ## name(ATTR, &i); \
		METHOD_RETURN(MAKE_INT(i)); \
	}
#define SIZE_ATTR(name) \
	if(!strcmp(field, #name)) { \
		pthread_attr_get ## name(ATTR, &size); \
		METHOD_RETURN(MAKE_INT(size)); \
	}
METHOD_RESULT native_attr_pthreadattr METHOD_PARAMS {
	char *field = obj_to_cstring(argv[1]);
	size_t size;
	int i;
	// TODO: check exit statuses maybe?
	// TODO: other attributes
	// TODO: PTHREAD_INHERIT_SCHED and PTHREAD_EXPLICIT_SCHED constants
	// TODO: PTHREAD_SCOPE_SYSTEM and PTHREAD_SCOPE_PROCESS constants
	INT_ATTR (detachstate);
	SIZE_ATTR(guardsize);
	INT_ATTR (inheritsched);
	INT_ATTR (scope);
	SIZE_ATTR(stacksize);
	// TODO: Throw exception
	METHOD_RETURN(MAKE_NULL);
}
#undef SIZE_ATTR
#undef INT_ATTR
#undef ATTR

// TODO: use vm_call METHOD_RESULT ?
// TODO: how to handle exceptions in the thread?
void *_pthread_start_routine(void *arg) {
	NGS_PTHREAD_INIT_INFO *init;
	CTX ctx;
	VALUE *result;
	METHOD_RESULT mr;
	VALUE *ts = NGS_MALLOC(sizeof(*ts));
	*ts = make_hash(4);
	pthread_setspecific(thread_local_key, ts);
	result = NGS_MALLOC(sizeof(*result));
	*result = make_array(2);
	init = (NGS_PTHREAD_INIT_INFO *)arg;
	ctx_init(&ctx);
	ARRAY_ITEMS(*result)[1] = MAKE_NULL;
	mr = vm_call(init->vm, &ctx, &ARRAY_ITEMS(*result)[1], init->f, 1, &init->arg);
	ARRAY_ITEMS(*result)[0] = MAKE_BOOL(mr == METHOD_EXCEPTION);
	return result;
}

METHOD_RESULT native_ll_thread_local METHOD_PARAMS {
	METHOD_RETURN(*(VALUE *)pthread_getspecific(thread_local_key));
}

METHOD_RESULT native_c_pthreadcreate_pthreadattr_startroutine_arg EXT_METHOD_PARAMS {
	VALUE pthread;
	VALUE ret;
	NGS_PTHREAD_INIT_INFO *init;
	int status;
	(void) ctx;
	init = NGS_MALLOC(sizeof(*init));
	assert(init);
	init->vm = vm;
	init->f = argv[1];
	init->arg = argv[2];
	pthread = make_pthread();
	status = pthread_create(&GET_PTHREAD(pthread), &GET_PTHREADATTR(argv[0]), &_pthread_start_routine, init);
	ret = make_array(2);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(status);
	ARRAY_ITEMS(ret)[1] = pthread;
	METHOD_RETURN(ret);
}

METHOD_RESULT native_c_pthreadjoin METHOD_PARAMS {
	VALUE ret;
	int status;
	VALUE *p;
	ret = make_array(2);
	status = pthread_join(GET_PTHREAD(argv[0]), (void **)&p);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(status);
	ARRAY_ITEMS(ret)[1] = status ? MAKE_NULL : *p;
	METHOD_RETURN(ret);
}

METHOD_RESULT native_c_pthreadattrinit METHOD_PARAMS  {
	int i = pthread_attr_init(&GET_PTHREADATTR(argv[0]));
	pthread_attr_setstacksize(&GET_PTHREADATTR(argv[0]), 8*1024*1024);
	METHOD_RETURN(MAKE_INT(i));
}

METHOD_RESULT native_c_pthreadmutexattrinit METHOD_PARAMS  {
	METHOD_RETURN(MAKE_INT(pthread_mutexattr_init(&GET_PTHREADMUTEXATTR(argv[0]))));
}

METHOD_RESULT native_c_pthreadmutexinit METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(pthread_mutex_init(&GET_PTHREADMUTEX(argv[0]), NULL)));
}

METHOD_RESULT native_c_pthreadmutexinit_pma METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(pthread_mutex_init(&GET_PTHREADMUTEX(argv[0]), &GET_PTHREADMUTEXATTR(argv[1]))));
}

METHOD_RESULT native_c_pthreadmutexlock METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_mutex_lock(&GET_PTHREADMUTEX(argv[0])))); }

METHOD_RESULT native_c_pthreadmutexunlock METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_mutex_unlock(&GET_PTHREADMUTEX(argv[0])))); }

METHOD_RESULT native_c_pthreadcondbroadcast METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_cond_broadcast(&GET_PTHREADCOND(argv[0])))); }
METHOD_RESULT native_c_pthreadconddestroy METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_cond_destroy(&GET_PTHREADCOND(argv[0])))); }
METHOD_RESULT native_c_pthreadcondinit METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_cond_init(&GET_PTHREADCOND(argv[0]), NULL))); }
METHOD_RESULT native_c_pthreadcondsignal METHOD_PARAMS { METHOD_RETURN(MAKE_INT(pthread_cond_signal(&GET_PTHREADCOND(argv[0])))); }
// TODO: pthread_cond_timedwait(3)
METHOD_RESULT native_c_pthreadcondwait METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(pthread_cond_wait(&GET_PTHREADCOND(argv[0]), &GET_PTHREADMUTEX(argv[1]))));
}

METHOD_RESULT native_c_pthreadself METHOD_PARAMS {
	VALUE pthread = make_pthread();
	(void) argv;
	GET_PTHREAD(pthread) = pthread_self();
	METHOD_RETURN(pthread);
}

// TODO: add ABI parameter
// TODO: better exception
METHOD_RESULT native_c_ffi_prep_cif EXT_METHOD_PARAMS {
	VALUE ret;
	size_t i;
	ffi_type **args;
	ffi_status status;

	ret = make_ffi_cif();

	args = NGS_MALLOC(sizeof(**args) * OBJ_LEN(argv[1]));
	for(i=0; i<OBJ_LEN(argv[1]); i++) {
		args[i] = GET_FFI_TYPE(ARRAY_ITEMS(argv[1])[i]);
	}
	if(FFI_OK != (status = ffi_prep_cif(&GET_FFI_CIF(ret), FFI_DEFAULT_ABI, OBJ_LEN(argv[1]), GET_FFI_TYPE(argv[0]), args))) {
		VALUE e;
		e = make_normal_type_instance(vm->Error);
		set_normal_type_instance_field(e, make_string("message"), make_string("Failed to ffi_prep_cif()"));
		set_normal_type_instance_field(e, make_string("status"), MAKE_INT(status));
		THROW_EXCEPTION_INSTANCE(e);
	}
	METHOD_RETURN(ret);
}

// WIP - start
// TODO: Exceptions instead of asserts
METHOD_RESULT native_c_ffi_call EXT_METHOD_PARAMS {
	// args:
	//   0 - cif
	//   1 - function
	//   2 - argv
	ffi_cif *cif;
	void **avalue;
	void *rvalue;
	unsigned i;
	void *tmp_ptr;
	(void) vm;
	(void) ctx;
	cif = &GET_FFI_CIF(argv[0]);
	assert(cif->nargs == OBJ_LEN(argv[2]));
	avalue = NGS_MALLOC(sizeof(*avalue) * cif->nargs);
	for(i=0; i<cif->nargs; i++) {
		avalue[i] = NGS_MALLOC(cif->arg_types[i]->size);
		if(cif->arg_types[i] == GET_FFI_TYPE(vm->c_ffi_type_string)) {
			tmp_ptr = NGS_MALLOC(sizeof(*tmp_ptr));
			if(IS_STRING(ARRAY_ITEMS(argv[2])[i])) {
				tmp_ptr = obj_to_cstring(ARRAY_ITEMS(argv[2])[i]);
				avalue[i] = &tmp_ptr;
				continue;
			}
			assert(0 == "ffi_call() - dunno how to make pointer");
		}
		assert(0 == "ffi_call() - dunno how to handle non-pointer types yet");
	}
	rvalue = NGS_MALLOC(cif->rtype->size);
	// o->base.val.ptr = dlsym(OBJ_DATA_PTR(argv[0]), obj_to_cstring(argv[1]));
	ffi_call(cif, ((CSYM_OBJECT *) argv[1].ptr)->base.val.ptr, rvalue, avalue);
	if(cif->rtype == GET_FFI_TYPE(vm->c_ffi_type_string)) {
		if(*(const char **)rvalue) {
			METHOD_RETURN(make_string(*(const char **)rvalue));
		} else {
			METHOD_RETURN(MAKE_NULL);
		}
	} else {
		assert(0 == "ffi_call() - dunno how to handle non-string types");
	}
	METHOD_RETURN(MAKE_NULL);
}
// WIP - end

METHOD_RESULT native_c_access METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(access(obj_to_cstring(argv[0]), GET_INT(argv[1]))));
}

#ifdef HAVE_POLL_H
// WIP
METHOD_RESULT native_c_poll METHOD_PARAMS {
	VALUE ret, revents;
	struct pollfd *fds;
	unsigned int i, len = OBJ_LEN(argv[0]);
	fds = NGS_MALLOC(sizeof(*fds) * len);
	for(i = 0; i<len; i++) {
		// TODO: assert ARRAY_ITEMS(argv[0])[i] has exactly two items
		fds[i].fd = GET_INT(ARRAY_ITEMS(ARRAY_ITEMS(argv[0])[i])[0]);
		fds[i].events = GET_INT(ARRAY_ITEMS(ARRAY_ITEMS(argv[0])[i])[1]);
	}
	int status = poll(fds, len, GET_INT(argv[1]));
	revents = make_array(len);
	for(i = 0; i<len; i++) {
		ARRAY_ITEMS(revents)[i] = MAKE_INT(fds[i].revents);
	}
	ret = make_array(2);
	ARRAY_ITEMS(ret)[0] = MAKE_INT(status);
	ARRAY_ITEMS(ret)[1] = revents;
	METHOD_RETURN(ret);
}
#endif

METHOD_RESULT native_id_pthread METHOD_PARAMS {
	unsigned char *p;
	char buf[1024];
	size_t i;
	p = (unsigned char *)&((PTHREAD_OBJECT *) argv[0].ptr)->val;
	for(i=0; i<sizeof(((PTHREAD_OBJECT *) argv[0].ptr)->val) && i<500; i++) {
		buf[i*2+0] = 'a' + ((p[i]) >> 4);
		buf[i*2+1] = 'a' + ((p[i]) & 0x0F);
	}
	buf[i*2] = '\0';
	METHOD_RETURN(make_string(buf));
}

METHOD_RESULT native_c_getpid METHOD_PARAMS {
	(void) argv;
	pid_t pid;
	pid = getpid();
	METHOD_RETURN(MAKE_INT(pid));
}

METHOD_RESULT native_c_getppid METHOD_PARAMS {
	(void) argv;
	pid_t pid;
	pid = getppid();
	METHOD_RETURN(MAKE_INT(pid));
}

METHOD_RESULT native_args EXT_METHOD_PARAMS {
	(void) vm;
	(void) argv;
	int i;
	int locals_idx = 0;
	int n = CLOSURE_OBJ_N_REQ_PAR(THIS_FRAME_CLOSURE) + CLOSURE_OBJ_N_OPT_PAR(THIS_FRAME_CLOSURE) + 2;
	VALUE closure = THIS_FRAME_CLOSURE;
	*result = make_hash(n);
	for(i=0; i<CLOSURE_OBJ_N_REQ_PAR(closure); i++) {
		set_hash_key(*result, CLOSURE_OBJ_PARAMS(closure)[i*2+0], LOCALS[locals_idx++]);
	}
	for(i=0; i<CLOSURE_OBJ_N_OPT_PAR(closure); i++) {
		set_hash_key(*result, CLOSURE_OBJ_PARAMS(closure)[CLOSURE_OBJ_N_REQ_PAR(closure)*2 + i*3 + 0], LOCALS[locals_idx++]);
	}
	i = CLOSURE_OBJ_N_REQ_PAR(closure)*2 + CLOSURE_OBJ_N_OPT_PAR(closure)*3;
	if(CLOSURE_OBJ_PARAMS_FLAGS(closure) & PARAMS_FLAG_ARR_SPLAT) {
		set_hash_key(*result, CLOSURE_OBJ_PARAMS(closure)[i+0], LOCALS[locals_idx++]);
		i+=2;
	}
	if(CLOSURE_OBJ_PARAMS_FLAGS(closure) & PARAMS_FLAG_HASH_SPLAT) {
		set_hash_key(*result, CLOSURE_OBJ_PARAMS(closure)[i+0], LOCALS[locals_idx++]);
		i+=2;
	}
	METHOD_RETURN(*result);
}

// Hackish
// TODO: something cleaner
METHOD_RESULT native_replace EXT_METHOD_PARAMS {
	(void) vm;
	(void) argv;
	size_t dst_size, src_size;
	if(!IS_OBJ(argv[0]) || !IS_OBJ(argv[1])) {
		VALUE exc;
		exc = make_normal_type_instance(vm->InvalidArgument);
		// TODO: better message phrasing
		set_normal_type_instance_field(exc, make_string("message"), make_string("Both replace() arguments must be objects, not tagged values"));
		set_normal_type_instance_field(exc, make_string("dst"), argv[0]);
		set_normal_type_instance_field(exc, make_string("src"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	if(IS_NORMAL_TYPE_INSTANCE(argv[0]) && IS_NORMAL_TYPE_INSTANCE(argv[1])) {
		memcpy(argv[0].ptr, argv[1].ptr, sizeof(OBJECT));
		METHOD_RETURN(argv[1]);
	}
	// TODO: throw InvalidArgument if only one IS_NORMAL_TYPE_INSTANCE
	if(OBJ_TYPE_NUM(argv[0]) != OBJ_TYPE_NUM(argv[1])) {
		VALUE exc;
		exc = make_normal_type_instance(vm->InvalidArgument);
		// TODO: better message phrasing
		set_normal_type_instance_field(exc, make_string("message"), make_string("Current implementation of replace() is limited to dst and src of same type when replacing a builtin type instance"));
		set_normal_type_instance_field(exc, make_string("dst"), argv[0]);
		set_normal_type_instance_field(exc, make_string("src"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	dst_size = NGS_SIZE(argv[0].ptr);
	src_size = NGS_SIZE(argv[1].ptr);
	// Not very clean but should be safe
	// TODO: review safety of such copy
	memcpy(argv[0].ptr, argv[1].ptr, MIN(dst_size, src_size));

	METHOD_RETURN(argv[1]);
}

METHOD_RESULT native_ll_resolve_global_variable EXT_METHOD_PARAMS {
	(void) ctx;
	METHOD_RETURN(MAKE_INT(get_global_index(vm, OBJ_DATA_PTR(argv[0]), OBJ_LEN(argv[0]))));
}

METHOD_RESULT native_ll_maybe_wrap METHOD_PARAMS {
	METHOD_RETURN(maybe_wrap(argv[0]));
};

METHOD_RESULT native_ll_is_global_variable_defined EXT_METHOD_PARAMS {
	GLOBAL_VAR_INDEX gvi = GET_INT(argv[0]);
	if(GET_INT(argv[0]) < 0 || gvi >= vm->globals_len) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_field(e, make_string("message"), make_string("Global variable with given index was not found"));
		set_normal_type_instance_field(e, make_string("key"), argv[0]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	METHOD_RETURN(MAKE_BOOL(IS_NOT_UNDEF(GLOBALS[gvi])));
}

METHOD_RESULT native_ll_set_global_variable EXT_METHOD_PARAMS {
	GLOBAL_VAR_INDEX gvi = GET_INT(argv[0]);
	if(GET_INT(argv[0]) < 0 || gvi >= vm->globals_len) {
		VALUE e;
		e = make_normal_type_instance(vm->IndexNotFound);
		set_normal_type_instance_field(e, make_string("message"), make_string("Global variable with given index was not found"));
		set_normal_type_instance_field(e, make_string("key"), argv[0]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	GLOBALS[gvi] = argv[1];
	METHOD_RETURN(argv[1]);
}

// http://www.pcre.org/original/doc/html/pcredemo.html
METHOD_RESULT native_c_pcre_compile EXT_METHOD_PARAMS {

	pcre *re;
	const char *error;
	int erroffset;

	re = pcre_compile(
		obj_to_cstring(argv[0]),    /* the pattern */
		GET_INT(argv[1]),           /* options */
		&error,                     /* for error message */
		&erroffset,                 /* for error offset */
		NULL                        /* use default character tables */
	);

	if(re == NULL) {
		VALUE exc;
		exc = make_normal_type_instance(vm->RegExpCompileFail);
		set_normal_type_instance_field(exc, make_string("message"), make_string(error));
		set_normal_type_instance_field(exc, make_string("regexp"), argv[0]);
		set_normal_type_instance_field(exc, make_string("offset"), MAKE_INT(erroffset));
		THROW_EXCEPTION_INSTANCE(exc);
	}

	*result = make_regexp();
	REGEXP_OBJECT_RE(*result) = re;

	return METHOD_OK;
}

METHOD_RESULT native_Str_regexp METHOD_PARAMS {
	(void) argv;
	*result = make_string("<RegExp>");
	return METHOD_OK;
}

#define OVECCOUNT 60 /* should be a multiple of 3 */
METHOD_RESULT native_c_pcre_exec METHOD_PARAMS {

	int rc;
	int ovector[OVECCOUNT];
	int i;

	rc = pcre_exec(
		REGEXP_OBJECT_RE(argv[0]), /* the compiled pattern */
		NULL,                      /* no extra data - we didn't study the pattern */
		OBJ_DATA_PTR(argv[1]) ? OBJ_DATA_PTR(argv[1]) : "",     /* the subject string */
		OBJ_LEN(argv[1]),          /* the length of the subject */
		GET_INT(argv[2]),          /* start offset */
		GET_INT(argv[3]),          /* options */
		ovector,                   /* output vector for substring information */
		OVECCOUNT                  /* number of elements in the output vector */
	);

	if(rc <= 0) {
		METHOD_RETURN(MAKE_INT(rc));
	}

	*result = make_array(rc*2);
	for(i=0; i < rc * 2; i++) {
		ARRAY_ITEMS(*result)[i] = MAKE_INT(ovector[i]);
	}

	return METHOD_OK;
}

// https://www.pcre.org/original/doc/html/pcre_fullinfo.html
METHOD_RESULT native_field_regexp EXT_METHOD_PARAMS {
	char *field = obj_to_cstring(argv[1]);
	pcre *re;
	re = REGEXP_OBJECT_RE(argv[0]);
	if(!strcmp(field, "options")) {
		unsigned long int option_bits;
		(void)pcre_fullinfo(re, NULL, PCRE_INFO_OPTIONS, &option_bits);
		METHOD_RETURN(MAKE_INT(option_bits));
	}

	if(!strcmp(field, "names")) {
		VALUE ret;
		int namecount, name_entry_size, i;
		unsigned char *name_table;

		(void)pcre_fullinfo(
			re,                   /* the compiled pattern */
			NULL,                 /* no extra data - we didn't study the pattern */
			PCRE_INFO_NAMECOUNT,  /* number of named substrings */
			&namecount            /* where to put the answer */
		);
		if(namecount <= 0) {
			METHOD_RETURN(make_hash(0));
		}

		(void)pcre_fullinfo(
			re,                       /* the compiled pattern */
			NULL,                     /* no extra data - we didn't study the pattern */
			PCRE_INFO_NAMETABLE,      /* address of the table */
			&name_table               /* where to put the answer */
		);

		(void)pcre_fullinfo(
			re,                       /* the compiled pattern */
			NULL,                     /* no extra data - we didn't study the pattern */
			PCRE_INFO_NAMEENTRYSIZE,  /* size of each entry in the table */
			&name_entry_size          /* where to put the answer */
		);

		ret = make_hash(namecount);
		for(i=0; i < namecount; i++, name_table += name_entry_size) {
			int n = (name_table[0] << 8) | name_table[1];
			set_hash_key(ret, make_string((const char *)&name_table[2]), MAKE_INT(n));
		}
		METHOD_RETURN(ret);
	}

	VALUE exc;
	exc = make_normal_type_instance(vm->FieldNotFound);
	set_normal_type_instance_field(exc, make_string("message"), make_string("RegExp does not have given field"));
	set_normal_type_instance_field(exc, make_string("container"), argv[0]);
	set_normal_type_instance_field(exc, make_string("key"), argv[1]);
	THROW_EXCEPTION_INSTANCE(exc);
}

METHOD_RESULT native_ord_str_int EXT_METHOD_PARAMS {
	int idx;
	idx = GET_INT(argv[1]);
	if(idx < 0 || idx >= (int) OBJ_LEN(argv[0])) {
		VALUE exc;
		exc = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_field(exc, make_string("message"), make_string("String index out of range"));
		set_normal_type_instance_field(exc, make_string("idx"), argv[1]);
		THROW_EXCEPTION_INSTANCE(exc);
	}
	METHOD_RETURN(MAKE_INT(((unsigned char *)OBJ_DATA_PTR(argv[0]))[idx]));
}

METHOD_RESULT native_chr_int_str METHOD_PARAMS {
	char c = (unsigned char) GET_INT(argv[0]);
	METHOD_RETURN(make_string_of_len(&c, 1));
}

METHOD_RESULT native_rand METHOD_PARAMS {
	(void) argv;
#if NGS_RAND_MAX < RAND_MAX
	METHOD_RETURN(MAKE_INT(random() % NGS_RAND_MAX));
#else
	METHOD_RETURN(MAKE_INT(random()));
#endif
}

METHOD_RESULT native_srand METHOD_PARAMS {
	srandom(GET_INT(argv[0]));
	METHOD_RETURN(MAKE_NULL);
}

METHOD_RESULT native_c_kill METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(kill(GET_INT(argv[0]), GET_INT(argv[1]))));
}

// TOOD: fdopendir() on supported platforms
METHOD_RESULT native_c_opendir METHOD_PARAMS {
	VALUE v;
	DIR *d = opendir(obj_to_cstring(argv[0]));
	if(!d) {
		METHOD_RETURN(MAKE_NULL);
	}
	v = make_DIR();
	DIR_OBJECT_DIR(v) = d;
	DIR_OBJECT_IS_OPEN(v) = 1;
	METHOD_RETURN(v);
}

// TODO: Special exception type
METHOD_RESULT native_c_readdir EXT_METHOD_PARAMS {
	VALUE ret;
	struct dirent *e;
	if(!DIR_OBJECT_IS_OPEN(argv[0])) {
		VALUE e;
		e = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_field(e, make_string("message"), make_string("Tried to c_readdir() on closed directory"));
		set_normal_type_instance_field(e, make_string("dirp"), argv[0]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	e = readdir(DIR_OBJECT_DIR(argv[0]));
	if(!e) {
		METHOD_RETURN(MAKE_NULL);
	}
	ret = make_hash(4);
	set_hash_key(ret, make_string("d_ino"), MAKE_INT(e->d_ino));
	set_hash_key(ret, make_string("d_name"), make_string(e->d_name));
	METHOD_RETURN(ret);
}

// TODO: Special exception type
METHOD_RESULT native_c_closedir EXT_METHOD_PARAMS {
	if(!DIR_OBJECT_IS_OPEN(argv[0])) {
		VALUE e;
		e = make_normal_type_instance(vm->InvalidArgument);
		set_normal_type_instance_field(e, make_string("message"), make_string("Tried to c_closedir() on closed directory"));
		set_normal_type_instance_field(e, make_string("dirp"), argv[0]);
		THROW_EXCEPTION_INSTANCE(e);
	}
	int ret = closedir(DIR_OBJECT_DIR(argv[0]));
	if(ret == 0) {
		DIR_OBJECT_IS_OPEN(argv[0]) = 0;
	}
	METHOD_RETURN(MAKE_INT(ret));
}

METHOD_RESULT native_c_chdir METHOD_PARAMS {
	int ret = chdir(obj_to_cstring(argv[0]));
	METHOD_RETURN(MAKE_INT(ret));
}

#define ELT(value) *p = MAKE_INT(value); p++;
#define MAKE_STAT_METHOD(cmd, arg_transform) \
METHOD_RESULT native_c_ ## cmd EXT_METHOD_PARAMS { \
	VALUE v; \
	VALUE *p; \
	struct stat buf; \
	int status; \
	(void) ctx; \
	status = cmd(arg_transform(argv[0]), &buf); \
	if(status != 0) { \
		METHOD_RETURN(MAKE_NULL); \
	} \
	v = make_normal_type_instance(vm->Stat); \
	OBJ_DATA(v) = make_array(13); /* Make sure to update this number if you add more ELT()s. Also add SETUP_TYPE_FIELD(Stat, ..., ...) below */ \
	p = ARRAY_ITEMS(OBJ_DATA(v)); \
	ELT(buf.st_dev); \
	ELT(buf.st_ino); \
	ELT(buf.st_mode); \
	ELT(buf.st_nlink); \
	ELT(buf.st_uid); \
	ELT(buf.st_gid); \
	ELT(buf.st_rdev); \
	ELT(buf.st_size); \
	ELT(buf.st_blksize); \
	ELT(buf.st_blocks); \
	ELT(buf.st_atime); \
	ELT(buf.st_mtime); \
	ELT(buf.st_ctime); \
	METHOD_RETURN(v); \
}
MAKE_STAT_METHOD(stat, obj_to_cstring)
MAKE_STAT_METHOD(lstat, obj_to_cstring)
MAKE_STAT_METHOD(fstat, GET_INT)
#undef MAKE_STAT_METHOD
#undef ELT

METHOD_RESULT native_c_socket METHOD_PARAMS {
	// socket(int domain, int type, int protocol)
	int ret = socket(GET_INT(argv[0]), GET_INT(argv[1]), GET_INT(argv[2]));
	METHOD_RETURN(MAKE_INT(ret));
}

METHOD_RESULT native_c_sockaddr METHOD_PARAMS {METHOD_RETURN(make_sockaddr()); };
METHOD_RESULT native_c_sockaddr_un METHOD_PARAMS {METHOD_RETURN(make_sockaddr_un()); };
METHOD_RESULT native_c_sockaddr_un_str METHOD_PARAMS {
	VALUE ret = make_sockaddr_un();
	GET_SOCKADDR_UN(ret).sun_family = AF_UNIX;
	strncpy(GET_SOCKADDR_UN(ret).sun_path, obj_to_cstring(argv[0]), 100); // TODO: replace 100
	METHOD_RETURN(ret);
};
METHOD_RESULT native_c_sockaddr_in METHOD_PARAMS {METHOD_RETURN(make_sockaddr_in()); };
METHOD_RESULT native_c_bind_Int_sockaddr_un METHOD_PARAMS {
	int ret = bind(GET_INT(argv[0]), (struct sockaddr *) &GET_SOCKADDR_UN(argv[1]), sizeof(GET_SOCKADDR_UN(argv[1])));
	METHOD_RETURN(MAKE_INT(ret));
};
METHOD_RESULT native_c_bind_Int_sockaddr_in METHOD_PARAMS {
	int ret = bind(GET_INT(argv[0]), (struct sockaddr *) &GET_SOCKADDR_IN(argv[1]), sizeof(GET_SOCKADDR_UN(argv[1])));
	METHOD_RETURN(MAKE_INT(ret));
}

METHOD_RESULT native_c_listen METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(listen(GET_INT(argv[0]), GET_INT(argv[1]))));
};

// accept(int socket, struct sockaddr *restrict address, socklen_t *restrict address_len)
METHOD_RESULT native_c_accept_sockaddr_un METHOD_PARAMS {
	socklen_t len = sizeof(GET_SOCKADDR_UN(argv[1]));
	int ret = accept(GET_INT(argv[0]), (struct sockaddr *) &GET_SOCKADDR_UN(argv[1]), &len);
	METHOD_RETURN(MAKE_INT(ret));
}

METHOD_RESULT native_gc_enable  METHOD_PARAMS { (void) argv; GC_enable();  METHOD_RETURN(MAKE_NULL); }
METHOD_RESULT native_gc_disable METHOD_PARAMS { (void) argv; GC_disable(); METHOD_RETURN(MAKE_NULL); }
METHOD_RESULT native_gc_get_parallel METHOD_PARAMS { (void) argv; METHOD_RETURN(MAKE_INT(GC_get_parallel())); }
METHOD_RESULT native_c_recvfrom_un METHOD_PARAMS {
	VALUE buf = make_string_of_len(NULL, GET_INT(argv[1]));
	socklen_t len = sizeof(GET_SOCKADDR_UN(argv[3]));
	ssize_t ret_code = recvfrom(GET_INT(argv[0]), OBJ_DATA_PTR(buf), GET_INT(argv[1]), /*flags*/ GET_INT(argv[2]), (struct sockaddr *) &GET_SOCKADDR_UN(argv[3]), &len);
	VALUE ret = make_array(0);
	array_push(ret, MAKE_INT(ret_code));
	if (ret_code > -1) {
		assert(ret_code <= GET_INT(argv[1]));
		OBJ_LEN(buf) = ret_code;
		array_push(ret, buf);
	} else {
		array_push(ret, MAKE_NULL);
	}
	METHOD_RETURN(ret);
};
METHOD_RESULT native_c_send METHOD_PARAMS {
	METHOD_RETURN(MAKE_INT(send(GET_INT(argv[0]), OBJ_DATA_PTR(argv[1]), OBJ_LEN(argv[1]), GET_INT(argv[2]))));
};

METHOD_RESULT native_Arr_mm METHOD_PARAMS {
	METHOD_RETURN(make_array_with_values(
		OBJ_LEN(MULTIMETHOD_METHODS(argv[0])),
		ARRAY_ITEMS(MULTIMETHOD_METHODS(argv[0]))
	));
}

METHOD_RESULT native_MultiMethod_arr METHOD_PARAMS {
	METHOD_RETURN(make_multimethod_from_array(argv[0]));
}

METHOD_RESULT native_sysconf_int METHOD_PARAMS {
	errno = 0;
	METHOD_RETURN(MAKE_INT(sysconf(GET_INT(argv[0]))));
}

GLOBAL_VAR_INDEX get_global_index(VM *vm, const char *name, size_t name_len) {
	GLOBAL_VAR_INDEX index;
	VALUE name_val;
	char *name_dup;
	int found;
	DEBUG_VM_RUN("entering get_global_index() vm=%p name=%.*s\n", vm, (int)name_len, name);
	index = check_global_index(vm, name, name_len, &found);
	if(found) {
		DEBUG_VM_RUN("leaving get_global_index() status=found vm=%p name=%.*s -> index=" GLOBAL_VAR_INDEX_FMT "\n", vm, (int)name_len, name, index);
		return index;
	}
	assert(vm->globals_len < (MAX_GLOBALS-1));
	name_val = make_string_of_len(name, name_len);
	index = vm->globals_len++;
	set_hash_key(vm->globals_indexes, name_val, MAKE_INT(index));
	GLOBALS[index] = MAKE_UNDEF;

	name_dup = NGS_MALLOC_ATOMIC(name_len+1);
	memcpy(name_dup, name, name_len);
	name_dup[name_len] = 0;
	vm->globals_names[index] = name_dup;

	DEBUG_VM_RUN("leaving get_global_index() status=new vm=%p name=%.*s -> index=" GLOBAL_VAR_INDEX_FMT "\n", vm, (int)name_len, name, var->index);
	return index;
}

VALUE _make_func(VM *vm, int pass_extra_params, char *name, void *func_ptr, int argc, va_list varargs) {
	int i;
	NATIVE_METHOD_OBJECT *func;
	VALUE *argv = NULL;
	func = NGS_MALLOC(sizeof(*func));
	func->base.type.num = T_NATIVE_METHOD;
	func->base.val.ptr = func_ptr;
	func->params.n_params_required = argc;
	func->params.n_params_optional = 0; /* currently none of builtins uses optional parameters */
	func->pass_extra_params = pass_extra_params;
	vm->last_doc_hash = make_hash(4);
	func->base.meta = make_hash(8);
		set_hash_key(func->base.meta, make_string("name"), make_string(name));
		set_hash_key(func->base.meta, make_string("doc"), vm->last_doc_hash);
	if(argc) {
		argv = NGS_MALLOC(argc * sizeof(VALUE) * 2);
		assert(argv);
		for(i=0; i<argc; i++) {
			// name:
			argv[i*2+0] = make_string(va_arg(varargs, char *));
			// type:
			argv[i*2+1] = (VALUE){.ptr = va_arg(varargs, NGS_TYPE *)};
		}
	}
	func->params.params = argv;
	return MAKE_OBJ(func);
}

void register_global_func(VM *vm, int pass_extra_params, char *name, void *func_ptr, int argc, ...) {
	size_t index;
	va_list args;
	va_start(args, argc);
	VALUE func = _make_func(vm, pass_extra_params, name, func_ptr, argc, args);
	va_end(args);
	index = get_global_index(vm, name, strlen(name));
	if(IS_MULMETHOD(GLOBALS[index])) {
		push_multimethod_method(GLOBALS[index], func);
		return;
	}
	if(IS_NGS_TYPE(GLOBALS[index])) {
		push_multimethod_method(NGS_TYPE_CONSTRUCTORS(GLOBALS[index]), func);
		return;
	}
	if(IS_UNDEF(GLOBALS[index])) {
		GLOBALS[index] = make_multimethod_with_value(func);
		return;
	}
	assert(0 == "register_global_func fail");
}

VALUE make_func(VM *vm, int pass_extra_params, char *name, void *func_ptr, int argc, ...) {
	va_list args;
	va_start(args, argc);
	VALUE func = _make_func(vm, pass_extra_params, name, func_ptr, argc, args);
	va_end(args);
	return func;
}

// TODO: consider array values (for separate lines or list items)
void _doc(VM *vm, char *k, char *v) {
	set_hash_key(vm->last_doc_hash, make_string(k), make_string(v));
}

void _doc_arr(VM *vm, char *k, ...) {
	va_list varargs;
	VALUE v;
	char *elt;
	v = make_array(0);
	va_start(varargs, k);
	while(1) {
		elt = va_arg(varargs, char *);
		if(!elt) break;
		array_push(v, make_string(elt));
	}
	va_end(varargs);
	set_hash_key(vm->last_doc_hash, make_string(k), v);
}

void set_global(VM *vm, const char *name, VALUE v) {
	size_t index;
	index = get_global_index(vm, name, strlen(name));
	GLOBALS[index] = v;
}

 VALUE register_builtin_type(VM *vm, const char *name, IMMEDIATE_TYPE native_type_id) {
	size_t index;
	VALUE ret;
	ret = make_normal_type(make_string(name));
	// Fixes for built-ins - start
	NGS_TYPE_ID(ret) = native_type_id;
	MULTIMETHOD_LEN(NGS_TYPE_CONSTRUCTORS(ret)) = 0;
	// Fixes for built-ins - end
	index = get_global_index(vm, name, strlen(name));
	assert(IS_UNDEF(GLOBALS[index]));
	GLOBALS[index] = ret;
	vm->last_doc_hash = make_hash(4);
	OBJ_META(ret) = make_hash(8);
		// set_hash_key(OBJ_META(ret), make_string("name"), make_string(name));
		set_hash_key(OBJ_META(ret), make_string("doc"), vm->last_doc_hash);
	return ret;
}

void vm_init(VM *vm, int argc, char **argv) {
	char **env, *equal_sign;
	VALUE env_hash, k, v;
	VALUE argv_array;
	int i;
	vm->bytecode = NULL;
	vm->bytecode_len = 0;
	vm->globals_indexes = make_hash(1024);
	vm->globals_len = 0;
	vm->globals = NGS_MALLOC(sizeof(*(vm->globals)) * MAX_GLOBALS);
	vm->globals_names = NGS_MALLOC(sizeof(char *) * MAX_GLOBALS);
	vm->regions = NULL;
	vm->regions_len = 0;
	vm->regions_allocated = 0;
	// Keep global functions registration in order.
	// This way the compiler can use globals_indexes as the beginning of
	// it's symbol table for globals.
#define MK_BUILTIN_TYPE(name, id) \
	vm->name = register_builtin_type(vm, #name, id);

#define MK_BUILTIN_TYPE_DOC(name, id, doc) \
	MK_BUILTIN_TYPE(name, id) \
	_doc(vm, "", doc);

	for(i=0; i<=MAX_VALUE_TAG_VALUE; i++) {
		vm->type_by_value_tag[i] = NULL;
	}

	for(i=0; i<=MAX_T_OBJ_TYPE_ID; i++) {
		vm->type_by_t_obj_type_id[i] = NULL;
	}

	set_global(vm, "VERSION", make_string(NGS_VERSION));

	// http://stackoverflow.com/questions/5919996/how-to-detect-reliably-mac-os-x-ios-linux-windows-in-c-preprocessor
	// nodejs:
	//   https://nodejs.org/api/process.html#process_process_platform
	//   deps/v8/include/v8config.h
	VALUE OS = make_hash(16);
	set_hash_key(OS, make_string("bsd"), MAKE_FALSE);
	set_hash_key(OS, make_string("darwin"), MAKE_FALSE);
	set_hash_key(OS, make_string("dragonflybsd"), MAKE_FALSE);
	set_hash_key(OS, make_string("freebsd"), MAKE_FALSE);
	set_hash_key(OS, make_string("linux"), MAKE_FALSE);
	set_hash_key(OS, make_string("netbsd"), MAKE_FALSE);
	set_hash_key(OS, make_string("openbsd"), MAKE_FALSE);
	set_hash_key(OS, make_string("posix"), MAKE_FALSE);
	set_hash_key(OS, make_string("qnx"), MAKE_FALSE);
	set_hash_key(OS, make_string("win"), MAKE_FALSE);
	#if defined(__APPLE__)
	set_hash_key(OS, make_string("darwin"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__linux__)
	set_hash_key(OS, make_string("linux"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
	set_hash_key(OS, make_string("bsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("freebsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__DragonFly__)
	set_hash_key(OS, make_string("bsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("dragonflybsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__NetBSD__)
	set_hash_key(OS, make_string("bsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("netbsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__OpenBSD__)
	set_hash_key(OS, make_string("bsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("openbsd"), MAKE_TRUE);
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	#elif defined(__QNXNTO__)
	set_hash_key(OS, make_string("posix"), MAKE_TRUE);
	set_hash_key(OS, make_string("qnx"), MAKE_TRUE);
	#elif defined(_WIN32)
	set_hash_key(OS, make_string("win"), MAKE_TRUE);
	#endif

	set_global(vm, "OS", OS);

	VALUE FEATURES = make_hash(4);
	set_global(vm, "FEATURES", FEATURES);

	MK_BUILTIN_TYPE_DOC(Null, T_NULL, "Null type. Has only one instance, null");
	vm->type_by_value_tag[V_NULL >> TAG_BITS] = &vm->Null;

	MK_BUILTIN_TYPE_DOC(Bool, T_BOOL, "Boolean type. The only objects are true and false");
	vm->type_by_value_tag[V_TRUE >> TAG_BITS] = &vm->Bool;
	vm->type_by_value_tag[V_FALSE >> TAG_BITS] = &vm->Bool;

	MK_BUILTIN_TYPE_DOC(Int, T_INT, "Integer type. On 64 bit platforms it's a 61 bit signed integer");
	// handled specially in value_type

	MK_BUILTIN_TYPE_DOC(Real, T_REAL, "Real/Float type. Equivalent to the 'double' type in C");
	vm->type_by_t_obj_type_id[T_REAL >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Real;

	MK_BUILTIN_TYPE_DOC(Str, T_STR, "String type. Contains sequential bytes.");
	vm->type_by_t_obj_type_id[T_STR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Str;

	MK_BUILTIN_TYPE(Arr, T_ARR);
	vm->type_by_t_obj_type_id[T_ARR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Arr;
	_doc(vm, "", "Array - ordered list of items accessed by zero-based index");
	_doc_arr(vm, "%EX",
		"x = [\"first\", \"second\", \"third\", \"fourth\"]",
		"",
		"echo(x)",
		"# Output:",
		"#   [first,second,third,fourth]",
		"",
		"echo(x.len())",
		"# Output:",
		"#   4",
		"",
		"echo(x[1])",
		"# Output:",
		"#   second",
		"",
		"echo(x[10])",
		"# ... Exception of type IndexNotFound occurred ...",
		NULL
	);


	MK_BUILTIN_TYPE_DOC(Fun, T_FUN, "Function type: native method, user defined method, or a multimethod");

		MK_BUILTIN_TYPE_DOC(UserDefinedMethod, T_CLOSURE, "UserDefinedMethod type. User-defined functions/methods are Closures");
		vm->type_by_t_obj_type_id[T_CLOSURE >> T_OBJ_TYPE_SHIFT_BITS] = &vm->UserDefinedMethod;

		MK_BUILTIN_TYPE_DOC(NativeMethod, T_NATIVE_METHOD, "Native method type. Methods implemented in C have this type.");
		vm->type_by_t_obj_type_id[T_NATIVE_METHOD >> T_OBJ_TYPE_SHIFT_BITS] = &vm->NativeMethod;

		MK_BUILTIN_TYPE_DOC(MultiMethod, T_MULMETHOD, "MultiMethod, container for methods.");
		vm->type_by_t_obj_type_id[T_MULMETHOD >> T_OBJ_TYPE_SHIFT_BITS] = &vm->MultiMethod;

	MK_BUILTIN_TYPE(Any, T_ANY);
	_doc_arr(vm, "",
		"Any type is parent type of all types. ",
		"All objects in NGS are of type Any. ",
		"F(x) ... is same as F(x:Any) ...",
		NULL
	);
		MK_BUILTIN_TYPE_DOC(BasicTypeInstance, T_BASICTI, "A type for objects of builtin types. Children types are not displayed as this type is specially optimized.");
		MK_BUILTIN_TYPE_DOC(NormalTypeInstance, T_NORMTI, "A type for objects of user-defined types. Children types are not displayed as this type is specially optimized.");

	MK_BUILTIN_TYPE_DOC(Seq, T_SEQ, "Unused type");

	MK_BUILTIN_TYPE_DOC(Type, T_TYPE, "Type for types. F f(t:Type) ...; f(Arr) ...");
	vm->type_by_t_obj_type_id[T_TYPE >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Type;
		MK_BUILTIN_TYPE_DOC(BasicType, T_BASICT, "Type for builtin types. F f(t:BasicType) ...; f(Arr)");
		MK_BUILTIN_TYPE_DOC(NormalType, T_NORMT, "Type for user-defined types. type T1; F f(t:NormalType) ...; f(T1)");

	MK_BUILTIN_TYPE(Hash, T_HASH);
	_doc_arr(vm, "",
		"Hash type. Maps unique keys to their values. ",
		"Key-Value pairs are stored and iterated in insertion order.",
		"Currently Hash type has several limitations: ",
		"Hash keys are hashed using internal hash() function which can not be overwritten. ",
		"The internal hash() multimethod exposed to NGS code but adding methods to it or setting \"hash\" to some other function ",
		"will not affect operation of Hashes. ",
		"Hash values are compared using internal is_equal() function which can not be overwritten. ",
		"Both hash() and is_equal() currently handle only Int, Str and arbitrary objects. ",
		"Comparison of arbitrary objects is done by comparing their addresses in memory.",
		NULL
	);
	_doc_arr(vm, "%EX",
		"x = {\"a\": 1, \"b\": 2}",
		"echo(x)",
		"# Output:",
		"#   {a=1, b=2}",
		"",
		"echo(x.keys())",
		"# Output:",
		"#   ['a','b']",
		"",
		"echo(x.values())",
		"# Output:",
		"#   [1,2]",
		"",
		"x = {\"a\": 1, \"b\": 2}",
		"x.a = 10",
		"echo(x)",
		"# Output:",
		"#   {a=10, b=2}",
		"",
		NULL
	);

	vm->type_by_t_obj_type_id[T_HASH >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Hash;

	MK_BUILTIN_TYPE(Namespace, T_NAMESPACE);
	_doc_arr(vm, "",
			 "Namespace type. Returned by 'ns' keyword. Inherits and functions mostly as Hash.",
			 NULL
	);
	_doc_arr(vm, "%EX",
			 "ns { F f(x) x * 2; }",
			 NULL
	);
	add_type_inheritance(vm->Namespace, vm->Hash);

	vm->type_by_t_obj_type_id[T_NAMESPACE >> T_OBJ_TYPE_SHIFT_BITS] = &vm->Namespace;

	MK_BUILTIN_TYPE_DOC(CLib, T_CLIB, "C library, result of dlopen(), not used yet");
	vm->type_by_t_obj_type_id[T_CLIB >> T_OBJ_TYPE_SHIFT_BITS] = &vm->CLib;

	MK_BUILTIN_TYPE_DOC(CSym, T_CSYM, "C symbol, result of dlsym(), not used yet");
	vm->type_by_t_obj_type_id[T_CSYM >> T_OBJ_TYPE_SHIFT_BITS] = &vm->CSym;

	MK_BUILTIN_TYPE(c_pthread_t, T_PTHREAD);
	vm->type_by_t_obj_type_id[T_PTHREAD >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_pthread_t;

	MK_BUILTIN_TYPE(c_pthread_attr_t, T_PTHREADATTR);
	vm->type_by_t_obj_type_id[T_PTHREADATTR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_pthread_attr_t;

	MK_BUILTIN_TYPE(c_pthread_mutex_t, T_PTHREADMUTEX);
	vm->type_by_t_obj_type_id[T_PTHREADMUTEX >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_pthread_mutex_t;

	MK_BUILTIN_TYPE(c_pthread_mutexattr_t, T_PTHREADMUTEXATTR);
	vm->type_by_t_obj_type_id[T_PTHREADMUTEXATTR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_pthread_mutexattr_t;

	MK_BUILTIN_TYPE(c_pthread_cond_t, T_PTHREADCOND);
	vm->type_by_t_obj_type_id[T_PTHREADCOND >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_pthread_cond_t;

	MK_BUILTIN_TYPE_DOC(c_ffi_type, T_FFI_TYPE, "Unfinished feature. Don't use!");
	vm->type_by_t_obj_type_id[T_FFI_TYPE >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_ffi_type;

	MK_BUILTIN_TYPE_DOC(c_ffi_cif, T_FFI_CIF, "Unfinished feature. Don't use!");
	vm->type_by_t_obj_type_id[T_FFI_CIF >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_ffi_cif;

	MK_BUILTIN_TYPE(RegExp, T_REGEXP);
	vm->type_by_t_obj_type_id[T_REGEXP >> T_OBJ_TYPE_SHIFT_BITS] = &vm->RegExp;

	MK_BUILTIN_TYPE_DOC(C_DIR, T_DIR, "C language DIR type for low level directory operations. Please do not use directly unless you are extending stdlib.");
	vm->type_by_t_obj_type_id[T_DIR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->C_DIR;

	MK_BUILTIN_TYPE(LLHashEntry, T_LL_HASH_ENTRY);
	vm->type_by_t_obj_type_id[T_LL_HASH_ENTRY >> T_OBJ_TYPE_SHIFT_BITS] = &vm->LLHashEntry;

	MK_BUILTIN_TYPE(c_sockaddr, T_SOCKADDR);
	vm->type_by_t_obj_type_id[T_SOCKADDR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_sockaddr;

	MK_BUILTIN_TYPE(c_sockaddr_un, T_SOCKADDR_UN);
	vm->type_by_t_obj_type_id[T_SOCKADDR_UN >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_sockaddr_un;
	add_type_inheritance(vm->c_sockaddr_un, vm->c_sockaddr);

	MK_BUILTIN_TYPE(c_sockaddr_in, T_SOCKADDR_IN);
	vm->type_by_t_obj_type_id[T_SOCKADDR_IN >> T_OBJ_TYPE_SHIFT_BITS] = &vm->c_sockaddr_in;
	add_type_inheritance(vm->c_sockaddr_in, vm->c_sockaddr);

	// *** Add new MKTYPE / MKSUBTYPE above this line ***

#undef MK_BUILTIN_TYPE

#define MKTYPE(name) \
	VALUE name; \
	(name) = make_normal_type(make_string(#name)); \
	set_global(vm, #name, name); \
	vm->name = name; \
	vm->last_doc_hash = make_hash(4); \
	OBJ_META(name) = make_hash(8); \
	set_hash_key(OBJ_META(name), make_string("doc"), vm->last_doc_hash);

#define MKSUBTYPE(name, parent) \
	MKTYPE(name); \
	add_type_inheritance(name, parent);

#define SETUP_TYPE_FIELD(name, field, idx) set_hash_key(NGS_TYPE_FIELDS(name), make_string(#field), MAKE_INT(idx));

	MKTYPE(NormalTypeConstructor);
	_doc(vm, "", "Default constructor for Normal types. Normal types are user-defined and some of the built-in types.");
	vm->type_by_t_obj_type_id[T_UTCTR >> T_OBJ_TYPE_SHIFT_BITS] = &vm->NormalTypeConstructor;

	MKTYPE(Exception);
	_doc(vm, "", "Represents exceptional situaution. All thrown things shouhld inherit Exception.");
	_doc(vm, "backtrace", "Automatic field set when creating Exception type objects (including sub-types, as long as super() is called.");

		MKSUBTYPE(Error, Exception);
		_doc(vm, "", "Represents an error. Usually more specific error types are used.");

			MKSUBTYPE(InternalError, Error);
			_doc(vm, "", "Represents an error which is likely to be NGS implementation bug.");

			MKSUBTYPE(LookupFail, Error);
			_doc(vm, "", "Represents an error of accessing non-existent element of a collection.");

				MKSUBTYPE(KeyNotFound, LookupFail);
				_doc(vm, "", "Represents an error of accessing non-existent key in a hash.");
				_doc_arr(vm, "%EX",
					"h = {}",
					"echo(h[\"a\"])",
					"# ... Exception of type KeyNotFound ...",
					NULL
				);

				MKSUBTYPE(IndexNotFound, LookupFail);
				_doc(vm, "", "Represents an error of out-of-bounds array index.");
				_doc_arr(vm, "%EX",
					"a = [10,20,30]",
					"echo(a[100])",
					"# ... Exception of type IndexNotFound ...",
					NULL
				);

					MKSUBTYPE(EmptyArrayFail, IndexNotFound);
					_doc(vm, "", "Represents an error of using an empty array for an operation that requires at least one element in the array.");
					_doc_arr(vm, "%EX",
						"a = []",
						"echo(shift(a))",
						"# ... Exception of type EmptyArrayFail ...",
						NULL
					);

				MKSUBTYPE(FieldNotFound, LookupFail);
				_doc(vm, "", "Represents an error of reading non-existent field of an object.");
				_doc_arr(vm, "%EX",
					"{",
					"  type T",
					"  t.a = 1",
					"  echo(t.b)",
					"}",
					"# ... Exception of type FieldNotFound ...",
					NULL
				);

				MKSUBTYPE(GlobalNotFound, LookupFail);
				_doc(vm, "", "Represents an error of accessing undefined global variable.");
				_doc_arr(vm, "%EX",
					"{ NO_SUCH_VAR }",
					"# ... Exception of type GlobalNotFound ...",
					NULL
				);

			MKSUBTYPE(UndefinedLocalVar, Exception);
			_doc(vm, "", "Represents an error of reading undefined local variable.");
			_doc_arr(vm, "%EX",
				"F f() {echo(a); a=1}",
				"f()",
				"# ... Exception of type UndefinedLocalVar ...",
				NULL
			);

			MKSUBTYPE(UndefinedUpVar, Exception);
			_doc(vm, "", "Represents an error of reading undefined upvar variable.");
			_doc_arr(vm, "%EX",
					 "F() { F inner() a; try { 1/0; a=1 }; inner }()()",
					 "# ... Exception of type UndefinedUpVar ...",
					 NULL
			);

	MKSUBTYPE(InvalidArgument, Error);
			_doc(vm, "", "Represents an error of calling a method with incorrect argument.");
			_doc_arr(vm, "%EX",
				"ord(\"ab\")",
				"# ... Exception of type InvalidArgument ...",
				NULL
			);

				MKSUBTYPE(DivisionByZero, InvalidArgument);
				_doc(vm, "", "Represents an error of dividing by zero.");
				_doc_arr(vm, "%EX",
					"echo(1 / 0)",
					"# ... Exception of type DivisionByZero ...",
					NULL
				);

			MKSUBTYPE(CompileFail, Error);
			_doc(vm, "", "Represents a compilation error.");
			_doc_arr(vm, "%EX",
				"compile(\"{ + }\", "")",
				"# ... Exception of type CompileFail ...",
				NULL
			);

			MKSUBTYPE(RegExpCompileFail, Error);
			_doc(vm, "", "Represents a regulat expression compilation error.");
			_doc_arr(vm, "%EX",
				"\"aaa\" ~ /+/",
				"# ... Exception of type RegExpCompileFail ...",
				NULL
			);

			MKSUBTYPE(CallFail, Error);
			_doc(vm, "", "Represents calling failure.");

				MKSUBTYPE(DontKnowHowToCall, CallFail);
				_doc(vm, "", "Represents calling failure when it is not known how to call the given object.");
				_doc_arr(vm, "%EX",
					"{ type T }",
					"t = T()",
					"t()",
					"# ... Exception of type DontKnowHowToCall ...",
					NULL
				);

				MKSUBTYPE(MethodNotFound, CallFail);
				_doc(vm, "", "Represents calling failure when arguments do not match any method implementation.");
				_doc_arr(vm, "%EX",
					"F f(x:Int) 1"
					"F f(x:Str) 2"
					"f(true)",
					"# ... Exception of type MethodNotFound ...",
					NULL
				);

				MKSUBTYPE(StackDepthFail, CallFail);
				_doc(vm, "", "Represents stack overflow error.");
				_doc_arr(vm, "%EX",
					"F f(x:Int) f(x+1)"
					"f(0)",
					"# ... Exception of type StackDepthFail ...",
					NULL
				);

				MKSUBTYPE(ArgsMismatch, CallFail);
				_doc(vm, "", "Represents calling failure due to arguments vs parameters mismatch.");
				_doc_arr(vm, "%EX",
					"f = F(x) \"blah\"",
					"f(10, 20)",
					"# ... Exception of type ArgsMismatch ...",
					NULL
				);

			MKSUBTYPE(SwitchFail, Error);
			_doc(vm, "", "Represents missing appropriate eswitch/econd/ematch clause.");
			_doc_arr(vm, "%EX",
				"{",
				"    eswitch 10 {",
				"        1 \"a\"",
				"        2 \"b\"",
				"    }",
				"}",
				"# ... Exception of type SwitchFail ...",
				NULL
			);

			MKSUBTYPE(DlopenFail, Error);
			_doc(vm, "", "Represents failure to open dynamically loaded library (feature is a work in progress).");

			MKSUBTYPE(DecodeFail, Error);
			_doc(vm, "", "Represents an error decoding a data.");

				MKSUBTYPE(JsonDecodeFail, DecodeFail);
				_doc(vm, "", "Represents an error decoding JSON data.");

			MKSUBTYPE(StackOverflow, Error);
			_doc(vm, "", "Represents a stack overflow error.");

	MKTYPE(Backtrace);
	_doc(vm, "", "Represents stack trace");
	_doc(vm, "frames", "Array of locations. Each element of the array is a Hash with \"ip\" and \"closure\" properties.");
	_doc_arr(vm, "%EX",
		"Backtrace().frames.each(echo)",
		"# {ip=4770, closure=<UserDefinedMethod <anonymous> at /etc/ngs/bootstrap.ngs:3>}",
		"# {ip=4153, closure=<UserDefinedMethod bootstrap_exception_catch_wrapper at /etc/ngs/bootstrap.ngs:205>}",
		"# {ip=3583, closure=<UserDefinedMethod bootstrap at /etc/ngs/bootstrap.ngs:111>}",
		"# {ip=116587, closure=<UserDefinedMethod <anonymous> at <command line -pi switch>:2>}",
		NULL
	);

	MKTYPE(CommandsPipeline);
	MKTYPE(CommandsPipe);
	MKTYPE(Command);

	MKTYPE(CommandRedir);
	_doc(vm, "", "Input/output redirection");

	// XXX: changing NGS_TYPE_FIELDS of InclusiveRange or ExclusiveRange
	//      in such a way that "start" is not 0 or "end" is not 1
	//      will break everything. TODO: make sure this can not be done by
	//      an NGS script.
	MKTYPE(NumRange);
		SETUP_TYPE_FIELD(NumRange, start, RANGE_FIELD_START);
		SETUP_TYPE_FIELD(NumRange, end, RANGE_FIELD_END);
		SETUP_TYPE_FIELD(NumRange, include_start, RANGE_FIELD_INCLUDE_START);
		SETUP_TYPE_FIELD(NumRange, include_end, RANGE_FIELD_INCLUDE_END);
		SETUP_TYPE_FIELD(NumRange, step, RANGE_FIELD_STEP);
	_doc(vm, "", "Numerical range");

	MKTYPE(Stat);
		SETUP_TYPE_FIELD(Stat, st_dev, 0U);
		SETUP_TYPE_FIELD(Stat, st_ino, 1U);
		SETUP_TYPE_FIELD(Stat, st_mode, 2U);
		SETUP_TYPE_FIELD(Stat, st_nlink, 3U);
		SETUP_TYPE_FIELD(Stat, st_uid, 4U);
		SETUP_TYPE_FIELD(Stat, st_gid, 5U);
		SETUP_TYPE_FIELD(Stat, st_rdev, 6U);
		SETUP_TYPE_FIELD(Stat, st_size, 7U);
		SETUP_TYPE_FIELD(Stat, st_blksize, 8U);
		SETUP_TYPE_FIELD(Stat, st_blocks, 9U);
		SETUP_TYPE_FIELD(Stat, st_atime, 10);
		SETUP_TYPE_FIELD(Stat, st_mtime, 11);
		SETUP_TYPE_FIELD(Stat, st_ctime, 12);
	_doc(vm, "", "Result of stat() or lstat()");

	MKTYPE(c_tm);
		SETUP_TYPE_FIELD(c_tm, tm_sec,   0U);
		SETUP_TYPE_FIELD(c_tm, tm_min,   1U);
		SETUP_TYPE_FIELD(c_tm, tm_hour,  2U);
		SETUP_TYPE_FIELD(c_tm, tm_mday,  3U);
		SETUP_TYPE_FIELD(c_tm, tm_mon,   4U);
		SETUP_TYPE_FIELD(c_tm, tm_year,  5U);
		SETUP_TYPE_FIELD(c_tm, tm_wday,  6U);
		SETUP_TYPE_FIELD(c_tm, tm_yday,  7U);
		SETUP_TYPE_FIELD(c_tm, tm_isdst, 8U);

	// "NgsStrImm${NgsStrExp}$*{NgsStrSplatExp}"
	MKTYPE(NgsStrComp);
		MKSUBTYPE(NgsStrCompImm, NgsStrComp);
		MKSUBTYPE(NgsStrCompExp, NgsStrComp);
		MKSUBTYPE(NgsStrCompSplatExp, NgsStrComp);

#undef SETUP_TYPE_FIELD
#undef MKSUBTYPE
#undef MKTYPE

	// Why is it here? Consider removing - start
	vm->eqeq = make_multimethod();
	set_global(vm, "==", vm->eqeq);
	// Why is it here? Consider removing - end

	register_global_func(vm, 0, "ll_maybe_wrap", &native_ll_maybe_wrap, 1, "v", vm->Any);

	register_global_func(vm, 0, "==",              &native_false,    2, "a", vm->Any, "b", vm->Any);
	_doc(vm, "", "Always false. Other == method implementations should compare types they understand. If none of them can handle the comparison, objects are considered non-equal.");
	_doc(vm, "%RET", "false");

	// Regex
	register_global_func(vm, 1, "c_pcre_compile", &native_c_pcre_compile,   2, "regexp", vm->Str,    "flags",   vm->Int);
	_doc(vm, "", "Compile regular expression. Uses PCRE_COMPILE(3). Do not use this function directly!");
	_doc(vm, "", "Throws RegExpCompileFail on errors.");
	_doc(vm, "%RET", "RegExp");

	register_global_func(vm, 0, "c_pcre_exec",    &native_c_pcre_exec,      4, "regexp", vm->RegExp, "subject", vm->Str, "offset", vm->Int, "options", vm->Int);
	_doc(vm, "", "Search string for regular expression. Uses PCRE_EXEC(3). Do not use this function directly!");
	_doc(vm, "%RET", "Int or Arr of Int");

	register_global_func(vm, 0, "Str",            &native_Str_regexp,       1, "regexp", vm->RegExp);
	_doc(vm, "", "Represents RegExp");
	_doc(vm, "%RET", "The string <RegExp>");

	register_global_func(vm, 1, ".",              &native_field_regexp,      2, "regexp", vm->RegExp, "field", vm->Str);
	_doc(vm, "", "Get fields of a RegExp. Throws FieldNotFound if field is not one of the allowed values. You should not use this directly. Use \"~\" and \"~~\" operators.");
	_doc(vm, "field", "\"options\" or \"names\"");
	_doc(vm, "%RET", "Int for \"options\". Hash of names/indexes of named groups for \"names\".");
	_doc_arr(vm, "%EX",
		"/abc/i.options  # 1 - case insensitive (C_PCRE_CASELESS)",
		"/(?P<name1>abc)/i.names  # Name to index Hash: {name1=1}",
		NULL
	);


	// special
	register_global_func(vm, 1, "args",            &native_args,            0);
	_doc(vm, "", "Get function arguments");
	_doc(vm, "%RET", "Hash");
	_doc_arr(vm, "%EX",
		"F f(x,y,z=100) args()",
		"f(1,2)  # {x=1, y=2, z=100}",
		NULL
	);

	register_global_func(vm, 1, "replace",         &native_replace,         2, "dst",    vm->Any,    "src", vm->Any);
	_doc(vm, "", "DISCOURAGED. Replace one object with another. dst and src must be of the same type.");
	_doc_arr(vm, "%EX",
		"a = [1,2,3]",
		"a.replace([4,5])",
		"a  # [4,5]",
		NULL
	);

	// global variables
	register_global_func(vm, 1, "ll_resolve_global_variable",    &native_ll_resolve_global_variable,     1, "name",   vm->Str);
	_doc(vm, "", "Do not use directly! Get global variable index by name.");
	register_global_func(vm, 1, "ll_is_global_variable_defined", &native_ll_is_global_variable_defined,  1, "idx",    vm->Int);
	_doc(vm, "", "Do not use directly! Check whether global variable is defined by index.");
	register_global_func(vm, 1, "ll_set_global_variable",        &native_ll_set_global_variable,         2, "idx",    vm->Int,    "val", vm->Any);
	_doc(vm, "", "Do not use directly! Set global variable by index.");

	// CLib and c calls
	register_global_func(vm, 1, "c_dlopen",        &native_c_dlopen_str_int,   2, "filename", vm->Str,        "flags",  vm->Int);
	_doc(vm, "", "Unfinished feature. Don't use!");
	register_global_func(vm, 0, "in",              &native_in_str_clib,        2, "symbol",   vm->Str,        "lib",    vm->CLib);
	_doc(vm, "", "Unfinished feature. Don't use!");
	register_global_func(vm, 1, "[]",              &native_index_get_clib_str, 2, "lib",      vm->CLib,       "symbol", vm->Str);
	_doc(vm, "", "Unfinished feature. Don't use!");
	register_global_func(vm, 1, "c_ffi_prep_cif",  &native_c_ffi_prep_cif ,    2, "rtype",    vm->c_ffi_type, "atypes", vm->Arr);
	_doc(vm, "", "Unfinished feature. Don't use!");
	register_global_func(vm, 1, "c_ffi_call",      &native_c_ffi_call,         3, "cif",      vm->c_ffi_cif,  "fn",     vm->CSym, "argv", vm->Arr);
	_doc(vm, "", "Unfinished feature. Don't use!");

	// threads
	register_global_func(vm, 1, "c_pthread_create",       &native_c_pthreadcreate_pthreadattr_startroutine_arg, 3, "attr", vm->c_pthread_attr_t, "start_routine", vm->UserDefinedMethod, "arg", vm->Any);
	_doc(vm, "", "Call PTHREAD_CREATE(3). Not recommended for direct calls, use Thread type instead.");
	_doc(vm, "%RET", "Arr with [Int, c_pthread_t]. Int is the status returned by pthread_create(). c_pthread_t is a thin wrapper around underlying pthread_t, returned by PTHREAD_CREATE(3)");
	_doc_arr(vm, "%EX",
		"F init(t:Thread, f:Fun, arg) {",
		"	thread_attr = c_pthread_attr_t()",
		"	c_pthread_attr_init(thread_attr)",
		"	create_result = c_pthread_create(thread_attr, f, arg)",
		"	code = create_result[0]",
		"	if code {",
		"		throw Error(\"Failed to c_pthread_create\")",
		"	}",
		"	t.thread = create_result[1]",
		"}",
		NULL
	);

	register_global_func(vm, 0, "c_pthread_join",         &native_c_pthreadjoin,        1, "thread", vm->c_pthread_t);
	_doc(vm, "", "Call PTHREAD_JOIN(3). Not recommended for direct calls, use Thread type instead.");
	_doc(vm, "%RET", "Arr with [Int, Any]. Int is the status returned by pthread_join(). Any is the value returned by the thread of status is 0, Any is null if status is non-zero.");
	_doc_arr(vm, "%EX",
		"F join(t:Thread) {",
		"	join_result = c_pthread_join(t.thread)",
		"	if join_result[0] {",
		"		throw Error(\"Failed to c_pthread_join\")",
		"	}",
		"	join_result[1]",
		"}",
		NULL
	);

	register_global_func(vm, 0, "c_pthread_attr_init",    &native_c_pthreadattrinit,    1, "attr",   vm->c_pthread_attr_t);
	_doc(vm, "", "Call PTHREAD_ATTR_INIT(3)");
	register_global_func(vm, 0, "c_pthread_mutexattr_init",   &native_c_pthreadmutexattrinit,    1, "attr",   vm->c_pthread_mutexattr_t);
	_doc(vm, "", "Call PTHREAD_MUTEXATTR_INIT(3)");
	register_global_func(vm, 0, "c_pthread_mutex_init",   &native_c_pthreadmutexinit_pma,   2, "mutex",  vm->c_pthread_mutex_t, "attr", vm->c_pthread_mutexattr_t);
	_doc(vm, "", "Call PTHREAD_MUTEX_INIT(3)");
	register_global_func(vm, 0, "c_pthread_mutex_init",   &native_c_pthreadmutexinit,   1, "mutex",  vm->c_pthread_mutex_t);
	_doc(vm, "", "Call PTHREAD_MUTEX_INIT(3) with NULL as second argument (attr)");
	register_global_func(vm, 0, "c_pthread_mutex_lock",   &native_c_pthreadmutexlock,   1, "mutex",  vm->c_pthread_mutex_t);
	_doc(vm, "", "Call PTHREAD_MUTEX_LOCK(3)");
	register_global_func(vm, 0, "c_pthread_mutex_unlock", &native_c_pthreadmutexunlock, 1, "mutex",  vm->c_pthread_mutex_t);
	_doc(vm, "", "Call PTHREAD_MUTEX_UNLOCK(3)");

	register_global_func(vm, 0, "c_pthread_cond_broadcast",   &native_c_pthreadcondbroadcast,   1, "cond",  vm->c_pthread_cond_t);
	_doc(vm, "", "Call PTHREAD_COND_BROADCAST(3)");
	register_global_func(vm, 0, "c_pthread_cond_destroy",   &native_c_pthreadconddestroy,   1, "cond",  vm->c_pthread_cond_t);
	_doc(vm, "", "Call PTHREAD_COND_DESTROY(3)");
	register_global_func(vm, 0, "c_pthread_cond_init",   &native_c_pthreadcondinit,   1, "cond",  vm->c_pthread_cond_t);
	_doc(vm, "", "Call PTHREAD_COND_INIT(3) with NULL as second argument (attr)");
	register_global_func(vm, 0, "c_pthread_cond_signal",   &native_c_pthreadcondsignal,   1, "cond",  vm->c_pthread_cond_t);
	_doc(vm, "", "Call PTHREAD_COND_SIGNAL(3)");
	register_global_func(vm, 0, "c_pthread_cond_wait",   &native_c_pthreadcondwait,   2, "cond",  vm->c_pthread_cond_t, "mutex", vm->c_pthread_mutex_t);
	_doc(vm, "", "Call PTHREAD_COND_WAIT(3)");

	register_global_func(vm, 0, "c_pthread_self",         &native_c_pthreadself,        0);
	_doc(vm, "", "Call PTHREAD_SELF(3)");
	register_global_func(vm, 0, "c_pthread_attr_t",       &native_c_pthreadattrt,       0);
	_doc(vm, "", "Create pthread attribute object");
	register_global_func(vm, 0, "c_pthread_mutex_t",      &native_c_pthreadmutext,      0);
	_doc(vm, "", "Create pthread mutex object");
	register_global_func(vm, 0, "c_pthread_mutexattr_t",  &native_c_pthreadmutexattrt,  0);
	_doc(vm, "", "Create pthread mutex attribute object");
	register_global_func(vm, 0, "c_pthread_mutexattr_settype",  &native_c_pthreadmutexattrsettype_pma_int,  2, "mutex", vm->c_pthread_mutexattr_t, "type", vm->Int);
	_doc(vm, "", "Call PTHREAD_MUTEXATTR_SETTYPE(3)");
	register_global_func(vm, 0, "c_pthread_cond_t",      &native_c_pthreadcondt,      0);
	_doc(vm, "", "Create pthread mutex object");

	register_global_func(vm, 0, "id",                     &native_id_pthread,           1, "thread", vm->c_pthread_t);
	_doc(vm, "", "Get pthread id as string of characters. This is opaque string which can be used for displaying and comparing to other pthreads ids.");
	_doc(vm, "%RET", "Str");

	register_global_func(vm, 0, ".",                      &native_attr_pthreadattr,     2, "pa", vm->c_pthread_attr_t,    "attr", vm->Str);
	_doc(vm, "", "Get pthread attribute. Currently returns null for unknown attributes. Will throw exceptions in future.");
	_doc(vm, "attr", "One of: detachstate, guardsize, inheritsched, stacksize.");

	register_global_func(vm, 0, "ll_thread_local",  &native_ll_thread_local,  0);
	_doc(vm, "", "Get thread-local storage");
	_doc(vm, "%RET", "Hash");

	// Native methods
	register_global_func(vm, 0, "params",   &native_params_nm,         1, "m",      vm->NativeMethod);
	_doc(vm, "", "Introspect method parameters");
	_doc_arr(vm, "%EX",
		"(+).Arr()[2].params().each(echo)",
		"# {name=a, type=<Type Int>}",
		"# {name=b, type=<Type Int>}",
		NULL
	);


	// Type
	// needed for switch
	register_global_func(vm, 0, "==",       &native_same_any_any,      2, "a",      vm->Type, "b", vm->Type);
	_doc(vm, "", "Compare types. Implemented as sameness comparison.");
	_doc(vm, "%EX", "type T; T==T  # true");
	_doc(vm, "%EX", "type T1; type T2; T1==T2  # false");

	// UserDefinedMethod
	register_global_func(vm, 0, "==",       &native_same_any_any,      2, "a",      vm->UserDefinedMethod, "b", vm->UserDefinedMethod);
	_doc(vm, "", "Compare closures. Implemented as sameness comparison.");
	_doc_arr(vm, "%EX",
		"F make_closure() { F(x) x + 1 }; make_closure()      == make_closure()       # false - different objects",
		"F make_closure() { F(x) x + 1 }; make_closure().ip() == make_closure().ip()  # true - same code",
		"f = F(x) x +1; f == f  # true - same instance",
		NULL
	);
	register_global_func(vm, 0, "params",   &native_params_closure,    1, "c",      vm->UserDefinedMethod);
	_doc(vm, "", "Introspect closure parameters.");
	_doc_arr(vm, "%EX",
		"... F the_one(something, pattern, body:Fun, found_more:Fun={null}, found_none:Fun={null}) ...",
		"the_one.Arr()[1].params().each(echo)"
		"# {name=something, type=<Type Any>}",
		"# {name=pattern, type=<Type Any>}",
		"# {name=body, type=<Type Fun>}",
		"# {name=found_more, type=<Type Fun>, dflt=<UserDefinedMethod <anonymous> at /usr/share/ngs/stdlib.ngs:198>}",
		"# {name=found_none, type=<Type Fun>, dflt=<UserDefinedMethod <anonymous> at /usr/share/ngs/stdlib.ngs:198>}",
		NULL
	);

	register_global_func(vm, 0, "ip",       &native_ip_closure,        1, "c",      vm->UserDefinedMethod);
	_doc(vm, "", "Get closure code instruction pointer.");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "f=F(x) x+1; f.ip()  # 116506");

	// MultiMethod
	register_global_func(vm, 0, "Arr",         &native_Arr_mm,           1, "mm",  vm->MultiMethod);
	_doc(vm, "", "Get methods of a MultiMethod");
	_doc(vm, "%RET", "Arr");

	register_global_func(vm, 0, "MultiMethod", &native_MultiMethod_arr,  1, "methods", vm->Arr);
	_doc(vm, "", "Construct MultiMethod from the given methods");
	_doc(vm, "%RET", "MultiMethod");

	// Int
	register_global_func(vm, 1, "Int",      &native_Int_real,           1, "r",    vm->Real);
	_doc(vm, "", "Convert Real (floating) number to Int. Floating part is truncated.");
	_doc(vm, "%RET", "Int");

	register_global_func(vm, 1, "Int",      &native_Int_str_int,        2, "s",    vm->Str,  "base", vm->Int);
	_doc(vm, "", "Convert Str to Int.");
	_doc_arr(vm, "%EX",
		"Int(\"100\", 2)  # 8",
		"Int(\"80\", 16)  # 128",
		NULL
	);

	// Real
	register_global_func(vm, 0, "+",        &native_plus_real_real,      2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Addition");
	register_global_func(vm, 0, "*",        &native_mul_real_real,       2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Multiplication");
	register_global_func(vm, 0, "/",        &native_div_real_real,       2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Division");
	register_global_func(vm, 0, "-",        &native_minus_real_real,     2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Subtraction");
	register_global_func(vm, 0, "<",        &native_less_real_real,      2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Less-than comparison");
	register_global_func(vm, 0, "<=",       &native_less_eq_real_real,   2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Less-than-or-equal comparison");
	register_global_func(vm, 0, ">",        &native_greater_real_real,   2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Greater-than comparison");
	register_global_func(vm, 0, ">=",       &native_greater_eq_real_real,2, "a",   vm->Real, "b", vm->Real);
	_doc(vm, "", "Greater-than-or-equal comparison");
	register_global_func(vm, 0, "==",       &native_eq_real_real,        2, "a",   vm->Real, "b", vm->Real);
	_doc_arr(vm, "",
		"Compare floating point numbers. Using this operator/function is not recommended.",
		NULL
	);
	_doc_arr(vm, "%EXTLINK",
		"http://how-to.wikia.com/wiki/Howto_compare_floating_point_numbers_in_the_C_programming_language Comparing floating point numbers (wikia)",
		NULL
	);
	register_global_func(vm, 0, "Str",      &native_Str_real,            1, "r",   vm->Real);
	_doc(vm, "", "Convert Real to Str");
	register_global_func(vm, 0, "Real",     &native_Real_int,            1, "n",   vm->Int);
	_doc(vm, "", "Convert Int to Real");
	register_global_func(vm, 0, "round",    &native_round_real,          1, "r",   vm->Real);
	_doc(vm, "", "Round a number");
	register_global_func(vm, 0, "trunc",    &native_trunc_real,          1, "r",   vm->Real);
	_doc(vm, "", "Truncate a number");
	register_global_func(vm, 0, "floor",    &native_floor_real,          1, "r",   vm->Real);
	_doc(vm, "", "Floor a number");
	_doc_arr(vm, "%EX",
		"floor(1.1)   # 1.0",
		"floor(-1.1)  # -2.0",
		NULL
	);
	register_global_func(vm, 0, "ceil",     &native_ceil_real,           1, "r",   vm->Real);
	_doc(vm, "", "Ceil a number");
	_doc_arr(vm, "%EX",
		"ceil(1.1)   # 2.0",
		"ceil(-1.1)  # -1.0",
		NULL
	);
	register_global_func(vm, 1, "c_pow",     &native_c_pow_real_real,        2, "base", vm->Real, "exponent", vm->Real);
	_doc(vm, "", "Raise to power");
	_doc_arr(vm, "%EX",
		"c_pow(2.0, 10.0)  # [0, 1024]",
		"c_pow(0.0, -1.0)  # [1, inf]",
		NULL
	);

	// OBJECT
	register_global_func(vm, 0, "meta", &native_meta, 1, "obj", vm->Any);
	_doc_arr(vm, "",
		"Get meta. Meta is auxiliary data slot. It is available on all non-immediate objects.",
		"The idea is to store additional information that will not get in your way in cases when you don't care about it.",
		NULL
	);
	register_global_func(vm, 0, "meta", &native_meta_any, 2, "obj", vm->Any, "v", vm->Any);
	_doc_arr(vm, "",
		"Set meta. Meta is auxiliary data slot. It is available on all non-immediate objects.",
		"The idea is to store additional information that will not get in your way in cases when you don't care about it.",
		NULL
	);

	// BasicType
	register_global_func(vm, 1, ".",        &native_get_field_bt_str,       2, "obj", vm->BasicType,          "field", vm->Str);
	_doc(vm, "", "Get BasicType (Int, Arr, Hash, ...) field. Throws FieldNotFound.");
	_doc(vm, "field", "Field to get. Currently only \"name\" and \"constructors\" are supported.");
	_doc(vm, "%AUTO", "obj.field");
	_doc(vm, "%RET", "Str for \"name\" and MultiMethod for \"constructors\".");
	_doc_arr(vm, "%EX",
		"Hash.name  # String: Hash",
		"Hash.constructors  # MultiMethod",
		NULL
	);

	// NormalType
	register_global_func(vm, 1, ".",        &native_get_field_nt_str,       2, "obj", vm->NormalType,         "field", vm->Str);
	_doc(vm, "", "Get NormalType (a type that is typically defined by user) field. Throws FieldNotFound.");
	_doc(vm, "field", "Field to get. Currently only \"name\", \"constructors\", \"parents\" and \"user\" are supported.");
	_doc(vm, "%AUTO", "obj.field");
	_doc(vm, "%RET", "Str for \"name\", MultiMethod for \"constructors\", Arr for \"parents\", Any for \"user\".");

	register_global_func(vm, 1, ".=",       &native_set_field_nt_str,       3, "obj", vm->NormalType,         "field", vm->Str, "v", vm->Any);
	_doc(vm, "", "Set NormalType (a type that is typically defined by user) field. Throws FieldNotFound.");
	_doc(vm, "field", "Field to set. Currently only \"user\" is supported.");
	_doc(vm, "%AUTO", "obj.field = v");
	_doc(vm, "%RET", "v");

	register_global_func(vm, 1, ".",        &native_get_field_nti_str,      2, "obj", vm->NormalTypeInstance, "field", vm->Str);
	_doc(vm, "", "Get NormalType (a type that is typically defined by user) instance field. Throws FieldNotFound.");
	_doc(vm, "%AUTO", "obj.field");
	_doc(vm, "%RET", "Any");
	_doc(vm, "%EX", "type T; t=T(); t.x=1; t.x  # 1");

	register_global_func(vm, 0, ".=",       &native_set_field_nti_str_any,  3, "obj", vm->NormalTypeInstance, "field", vm->Str, "v", vm->Any);
	_doc(vm, "", "Set Normal type (a type that is typically defined by user) instance field");
	_doc(vm, "%AUTO", "obj.field = v");
	_doc(vm, "%RET", "v");
	_doc(vm, "%EX", "type T; t=T(); t.x=1");

	register_global_func(vm, 0, "in",       &native_in_nti_str,            2, "field", vm->Str,               "obj", vm->NormalTypeInstance);
	_doc(vm, "", "Check whether NormalType (a type that is typically defined by user) instance has the given field.");
	_doc(vm, "%AUTO", "field in obj");
	_doc(vm, "%RET", "Bool");
	_doc(vm, "%EX", "type T; t=T(); t.x=1; \"x\" in t  # true");

	register_global_func(vm, 0, "inherit",  &native_inherit_t_t,         2, "t",   vm->Type,         "parent", vm->Type);
	_doc(vm, "", "Make t inherit from parent. Do not use directly. Use \"type MyType(parent)\".");
	_doc(vm, "%RET", "t");
	_doc_arr(vm, "%EX",
		"type NotImplemented",
		"NotImplemented.inherit(Exception)",
		NULL
	);

	// Type
	register_global_func(vm, 0, "Type", &native_type_str_doc_ns      , 3, "name", vm->Str, "doc", vm->Any, "ns", vm->Any);
	_doc(vm, "", "Create a new type. Do not use directly.");
	_doc(vm, "%AUTO", "type MyType");

	register_global_func(vm, 1, "Type",   &native_typeof_any        ,1, "x",      vm->Any);
	_doc(vm, "", "Returns type of the given object");
	_doc(vm, "x", "Object. Currently only objects of NormalType are supported.");

	// low level file operations
	register_global_func(vm, 0, "c_dup2",   &native_c_dup2_int_int,    2, "oldfd",    vm->Int, "newfd", vm->Int);
	_doc(vm, "", "Duplicate a file descriptor. Uses DUP2(2).");
	_doc(vm, "%RET", "Int - file descriptor or -1");
	register_global_func(vm, 0, "c_open",   &native_c_open_str_str,    2, "pathname", vm->Str, "flags", vm->Str);
	_doc(vm, "", "Open a file. Uses OPEN(2).");
	_doc(vm, "flags", "r - O_RDONLY; w - O_WRONLY | O_CREAT | O_TRUNC; a - O_WRONLY | O_CREAT | O_APPEND");
	_doc(vm, "%RET", "Int - file descriptor or -1");
	register_global_func(vm, 0, "c_realpath",  &native_c_realpath_str, 1, "path",     vm->Str);
	_doc(vm, "", "Real path. Uses REALPATH(3).");
	_doc(vm, "%RET", "Str or null");
	register_global_func(vm, 0, "c_close",  &native_c_close_int,       1, "fd",       vm->Int);
	_doc(vm, "", "Close a file. Uses CLOSE(2).");
	_doc(vm, "%RET", "Int - zero on success or -1");
	register_global_func(vm, 0, "c_read",   &native_c_read_int_int,    2, "fd",       vm->Int, "count", vm->Int);
	_doc(vm, "", "Read from a file. Uses READ(2).");
	_doc(vm, "count", "Maximal number of bytes to read.");
	_doc(vm, "%RET", "Arr of two elements: Int - number of bytes read or -1, Str - the read bytes");
	register_global_func(vm, 0, "c_write",  &native_c_write_int_str,   2, "fd",       vm->Int, "s",     vm->Str);
	_doc(vm, "", "Write to a file. Uses WRITE(2).");
	_doc(vm, "%RET", "Int - number of bytes written or -1");
#ifdef HAVE_POLL_H
	register_global_func(vm, 0, "c_poll",   &native_c_poll,            2, "fds_evs",  vm->Arr, "timeout", vm->Int);
	set_hash_key(FEATURES, make_string("poll"), MAKE_TRUE);
#else
	set_hash_key(FEATURES, make_string("poll"), MAKE_FALSE);
#endif
	// TODO DOC
	register_global_func(vm, 1, "c_lseek",  &native_c_lseek_int_int_str,3,"fd",       vm->Int, "offset", vm->Int, "whence", vm->Str);
	_doc(vm, "", "Call LSEEK(2).");
	_doc(vm, "whence", "One of: set, cur, end");
	_doc(vm, "%RET", "Int: new offset or -1");
	register_global_func(vm, 0, "c_isatty", &native_c_isatty,           1,"fd",       vm->Int);
	_doc(vm, "", "Check if file descriptor refers to a TTY device. Uses ISATTY(3).");
	_doc(vm, "%RET", "Int: 1 or 0");
	register_global_func(vm, 0, "c_opendir", &native_c_opendir,         1,"name",     vm->Str);
	_doc(vm, "", "Call OPENDIR(3)");
	register_global_func(vm, 1, "c_readdir", &native_c_readdir,         1,"dirp",     vm->C_DIR);
	_doc(vm, "", "Call READDIR(3)");
	register_global_func(vm, 1, "c_closedir",&native_c_closedir,        1,"dirp",     vm->C_DIR);
	_doc(vm, "", "Call CLOSEDIR(3)");
	register_global_func(vm, 0, "c_chdir",   &native_c_chdir,           1,"dir",      vm->Str);
	_doc(vm, "", "Call CHDIR(2)");

	register_global_func(vm, 1, "c_stat",    &native_c_stat,            1,"pathname", vm->Str);
	_doc(vm, "", "Call STAT(2)");

	register_global_func(vm, 1, "c_lstat",   &native_c_lstat,           1,"pathname", vm->Str);
	_doc(vm, "", "Call LSTAT(2)");
	_doc(vm, "%EX", "c_lstat(\"/tmp\")  # <Stat st_dev=... st_ino=... st_mode=... ...>");

	register_global_func(vm, 1, "c_fstat",   &native_c_fstat,           1,"fd",       vm->Int);
	_doc(vm, "", "Call FSTAT(2)");

	// sockets - types
	register_global_func(vm, 0, "c_sockaddr", &native_c_sockaddr, 0);
	register_global_func(vm, 0, "c_sockaddr_un", &native_c_sockaddr_un, 0);
	register_global_func(vm, 0, "c_sockaddr_in", &native_c_sockaddr_in, 0);

	// sockets - functions
	register_global_func(vm, 0, "c_socket", &native_c_socket, 3, "domain", vm->Int, "type", vm->Int, "protocol", vm->Int);
	register_global_func(vm, 0, "c_sockaddr_un", &native_c_sockaddr_un_str, 1, "path", vm->Str);
	register_global_func(vm, 0, "c_bind", &native_c_bind_Int_sockaddr_un, 2, "socket", vm->Int, "address", vm->c_sockaddr_un);
	register_global_func(vm, 0, "c_bind", &native_c_bind_Int_sockaddr_in, 2, "socket", vm->Int, "address", vm->c_sockaddr_in);
	register_global_func(vm, 0, "c_accept", &native_c_accept_sockaddr_un, 2, "socket", vm->Int, "address", vm->c_sockaddr_un);
	register_global_func(vm, 0, "c_listen", &native_c_listen, 2, "socket", vm->Int, "backlog", vm->Int);
	register_global_func(vm, 0, "c_recvfrom", &native_c_recvfrom_un, 4, "socket", vm->Int, "length", vm->Int, "flags", vm->Int, "address", vm->c_sockaddr_un);
	register_global_func(vm, 0, "c_send", &native_c_send, 3, "socket", vm->Int, "buffer", vm->Str, "flags", vm->Int);

	// low level misc
	register_global_func(vm, 0, "c_access", &native_c_access,          2, "pathname", vm->Str, "mode", vm->Int);
	_doc(vm, "", "Call ACCESS(2)");

	register_global_func(vm, 0, "c_exit",   &native_c_exit_int,        1, "status",   vm->Int);
	_doc(vm, "", "Call EXIT(3). Don't use directly unless you must. Use FatalError exception.");

	register_global_func(vm, 0, "c_fork",   &native_c_fork,            0);
	_doc(vm, "", "Call FORK(2)");

	register_global_func(vm, 0, "c_getpid", &native_c_getpid,          0);
	_doc(vm, "", "Call GETPID(2)");

	register_global_func(vm, 0, "c_getppid",&native_c_getppid,         0);
	_doc(vm, "", "Call GETPPID(2)");

	register_global_func(vm, 0, "c_pipe",   &native_c_pipe,            0);
	_doc(vm, "", "Call PIPE(2)");
	_doc(vm, "%RET", "Array with 3 items: error code, reading end file descriptor, writing end file descriptor");

	register_global_func(vm, 0, "c_waitpid",&native_c_waitpid,         1, "pid",      vm->Int);
	_doc(vm, "", "Call WAITPID(2)");
	register_global_func(vm, 0, "c_execve", &native_c_execve,          3, "filename", vm->Str, "argv", vm->Arr, "envp", vm->Arr);
	_doc(vm, "", "Call EXECVE(2)");
	register_global_func(vm, 0, "C_WEXITSTATUS", &native_C_WEXITSTATUS,1, "status",   vm->Int);
	_doc(vm, "", "Use WEXITSTATUS macro.");
	register_global_func(vm, 0, "C_WTERMSIG", &native_C_WTERMSIG,      1, "status",   vm->Int);
	_doc(vm, "", "Use WTERMSIG macro.");

	register_global_func(vm, 0, "c_errno",     &native_c_errno,    0);
	_doc(vm, "", "Use errno macro.");
	register_global_func(vm, 0, "c_strerror",  &native_c_strerror,     1, "errnum",   vm->Int);
	_doc(vm, "", "Call STRERROR(2)");

	register_global_func(vm, 0, "c_strcasecmp", &native_c_strcasecmp,  2, "a",   vm->Str,  "b",   vm->Str);
	_doc(vm, "", "Call STRCASECMP(3)");
	register_global_func(vm, 0, "c_strcmp",     &native_c_strcmp,      2, "a",   vm->Str,  "b",   vm->Str);
	_doc(vm, "", "Call STRCMP(3)");

	register_global_func(vm, 0, "c_kill",       &native_c_kill,        2, "pid", vm->Int,  "sig", vm->Int);
	_doc(vm, "", "Call KILL(2). Global variable SIGNALS contains mapping between signals' names and values.");

	// boolean
	register_global_func(vm, 0, "==",       &native_eq_bool_bool,      2, "a",   vm->Bool, "b", vm->Bool);
	_doc(vm, "", "Compare booleans");
	register_global_func(vm, 0, "not",      &native_not_bool,          1, "x",   vm->Bool);
	_doc(vm, "", "Invert boolean");

	// array
	register_global_func(vm, 0, "+",        &native_plus_arr_arr,            2, "a",   vm->Arr, "b", vm->Arr);
	_doc(vm, "", "Array concatenation.");
	_doc(vm, "%RET", "Arr");
	_doc(vm, "%EX", "[1,2]+[3,4]  # [1,2,3,4]");

	register_global_func(vm, 0, "push",     &native_push_arr_any,            2, "arr", vm->Arr, "v", vm->Any);
	_doc(vm, "", "Append item to an array.");
	_doc(vm, "%RET", "arr");
	_doc(vm, "%EX", "a=[1,2]; a.push(3)  # a is now [1,2,3]");

	register_global_func(vm, 1, "pop",      &native_pop_arr,                 1, "arr", vm->Arr);
	_doc(vm, "", "Pop item from an array. Removes last item in array and returns it. Throws EmptyArrayFail.");
	_doc(vm, "%RET", "Any");
	_doc(vm, "%EX", "a=[1,2]; a.pop()  # 2, a is now [1]");

	// TODO [doc]: examples
	register_global_func(vm, 1, "shift",    &native_shift_arr,               1, "arr", vm->Arr);
	_doc(vm, "", "Get the first element and remove it from the array. Throws EmptyArrayFail if there are no elements in the array.");
	_doc(vm, "%RET", "Any");

	register_global_func(vm, 0, "shift",    &native_shift_arr_any,           2, "arr", vm->Arr, "dflt", vm->Any);
	_doc(vm, "", "Get the first element and remove it from the array. Returns dlft if there are no elements in the array.");
	_doc(vm, "%RET", "Any");

	register_global_func(vm, 0, "len",      &native_len,                     1, "arr", vm->Arr);
	_doc(vm, "", "Get number of elements in the array");
	_doc(vm, "%RET", "Int");

	register_global_func(vm, 1, "[]",       &native_index_get_arr_range,     2, "arr", vm->Arr, "range", vm->NumRange);
	_doc(vm, "", "Get array elements at specified indexes.");
	_doc(vm, "r", "NumRange with positive .start and .end");
	_doc(vm, "%RET", "Arr");

	register_global_func(vm, 1, "[]=",      &native_index_set_arr_range_arr, 3, "arr", vm->Arr, "range", vm->NumRange, "replacement", vm->Arr);
	_doc(vm, "", "Set array elements at specified indexes.");
	_doc(vm, "r", "NumRange with positive .start and .end");
	_doc(vm, "%RET", "replacement");

	register_global_func(vm, 1, "[]",       &native_index_get_arr_int,       2, "arr", vm->Arr, "idx", vm->Int);
	_doc(vm, "", "Get element at the given index or throw IndexNotFound if the index is out of range (element at the given index does not exist).");
	_doc(vm, "%RET", "Any");

	register_global_func(vm, 1, "[]=",      &native_index_set_arr_int_any,   3, "arr", vm->Arr, "idx", vm->Int, "v", vm->Any);
	_doc(vm, "", "Set element at the given index or throw IndexNotFound if the index is out of range.");
	_doc(vm, "%RET", "v");

	register_global_func(vm, 1, "join",     &native_join_arr_str,            2, "arr", vm->Arr, "s", vm->Str);
	_doc(vm, "", "Join strings using s as glue");
	_doc(vm, "arr", "Arr of Str");

	register_global_func(vm, 0, "copy",     &native_copy_arr,                1, "arr", vm->Arr);
	_doc(vm, "", "Shallow copy of arr");
	_doc(vm, "%RET", "Arr");

	// string
	// TODO: other string comparison operators
	register_global_func(vm, 0, "len",      &native_len,                     1, "s",   vm->Str);
	_doc(vm, "", "Get Str length in bytes");
	_doc(vm, "%RET", "Int");

	register_global_func(vm, 0, "==",       &native_eq_str_str,              2, "a",   vm->Str, "b", vm->Str);
	_doc(vm, "", "Compare strings");

	register_global_func(vm, 0, "pos",      &native_pos_str_str_int,         3, "haystack", vm->Str, "needle", vm->Str, "start", vm->Int);
	_doc(vm, "", "Find substring position");
	_doc(vm, "start", "Non-negative Int, position where the search starts");
	_doc(vm, "%RET", "Int or null. Not -1 as in many languages");

	register_global_func(vm, 1, "[]",       &native_index_get_str_range,     2, "s", vm->Str, "range", vm->NumRange);
	_doc(vm, "", "Get substring");
	_doc(vm, "%EX", "\"abcd\"[1..3]  # \"bc\"");

	register_global_func(vm, 1, "[]=",      &native_index_set_str_range_str, 3, "s", vm->Str, "range", vm->NumRange, "replacement", vm->Str);
	_doc(vm, "", "Change substring");
	_doc(vm, "%RET", "replacement");
	_doc(vm, "%EX", "s=\"abcd\"; s[1..3]=\"X\"; s  # \"aXd\"");

	register_global_func(vm, 1, "ord",      &native_ord_str_int,             2, "s", vm->Str, "idx", vm->Int);
	_doc(vm, "", "Get character (byte) ordinal value. Throws InvalidArgument if idx is not pointing into s.");
	_doc(vm, "idx", "Index of the character to get value of");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "ord(\"A\", 0)  # 65 on my machine");

	register_global_func(vm, 0, "chr",      &native_chr_int_str,             1, "code", vm->Int);
	_doc(vm, "", "Get character (byte) by it's ordinal value.");
	_doc(vm, "%RET", "Str of length 1 (byte).");
	_doc(vm, "%EX", "chr(65)  # \"A\" on my machine");

	// int
	register_global_func(vm, 0, "+",        &native_plus_int_int,      2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Addition");
	register_global_func(vm, 0, "*",        &native_mul_int_int,       2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Multiplication");
	register_global_func(vm, 1, "/",        &native_div_int_int,       2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Division");
	register_global_func(vm, 1, "%",        &native_mod_int_int,       2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Modulus");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "10 % 3  # 1");
	register_global_func(vm, 0, "-",        &native_minus_int_int,     2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Subtraction");
	register_global_func(vm, 0, "<",        &native_less_int_int,      2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Less-than comparison");
	register_global_func(vm, 0, "<=",       &native_less_eq_int_int,   2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Less-than-or-equal comparison");
	register_global_func(vm, 0, ">",        &native_greater_int_int,   2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Greater-than comparison");
	register_global_func(vm, 0, ">=",       &native_greater_eq_int_int,2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Greater-than-or-equal comparison");
	register_global_func(vm, 0, "==",       &native_eq_int_int,        2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Equality comparison");
	register_global_func(vm, 0, "band",     &native_band_int_int,      2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Bitwise and");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "7.band(5)  # 5");
	register_global_func(vm, 0, "bor",      &native_bor_int_int,       2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Bitwise or");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "1.bor(8)  # 9");
	register_global_func(vm, 0, "bxor",     &native_bxor_int_int,      2, "a",   vm->Int, "b", vm->Int);
	_doc(vm, "", "Bitwise xor");
	_doc(vm, "%RET", "Int");
	_doc(vm, "%EX", "15.bxor(1)  # 14");

	// random
	register_global_func(vm, 0, "rand",     &native_rand,            0);
	_doc(vm, "", "Get random number between 0 and RAND_MAX-1. Uses RANDOM(3).");
	_doc(vm, "%RET", "Int");

	register_global_func(vm, 0, "srand",    &native_srand,           1, "seed", vm->Int);
	_doc(vm, "%RET", "Unspecified");
	_doc(vm, "", "Seed random generator. Uses SRANDOM(3).");

	// misc
	register_global_func(vm, 0, "===",      &native_same_any_any,      2, "a",   vm->Any, "b", vm->Any);
	_doc(vm, "", "Sameness comparison.");

	register_global_func(vm, 0, "dump",     &native_dump_any,          1, "obj", vm->Any);
	_doc(vm, "", "Low-level data structure dump. Used for debugging NGS itself.");

	register_global_func(vm, 0, "Bool",     &native_Bool_any,          1, "x",   vm->Any);
	_doc(vm, "", "Convert to Bool. Str, Arr and Hash of non-zero size return true. Bool returns as is. Null returns false. Int returns true if it is not zero.");
	_doc(vm, "x", "Bool or Int or Str or Arr or Hash or Null");
	_doc(vm, "%RET", "Bool");

	register_global_func(vm, 0, "Str",      &native_Str_int,           1, "n",   vm->Int);
	_doc(vm, "", "Convert Int to Str");

	register_global_func(vm, 1, "is",       &native_is_any_type,       2, "obj", vm->Any, "t", vm->Type);
	_doc(vm, "", "Check whether obj is of type t. Uses same function that is used for matching arguments with method parameters when calling a method.");
	_doc(vm, "%RET", "Bool");
	_doc_arr(vm, "%EX",
		"1 is Int  # true",
		"[] is Arr  # true",
		"[] is Int  # false",
		NULL
	);

	register_global_func(vm, 1, "compile",  &native_compile_str_str,   2, "code",vm->Str, "fname", vm->Str);
	_doc(vm, "", "Compile NGS source to bytecode.");
	_doc(vm, "fname", "Source file name for backtraces and error messages.");
	_doc(vm, "%RET", "Str - bytecode");
	_doc_arr(vm, "%EX",
		"# From bootstrap.ngs, require() definition",
		"program_text = fetch(fname)",
		"program_bytecode = compile(program_text, fname)",
		"program_func = load(program_bytecode, \"require()d file: $fname\")",
		"ret = program_func()",
		NULL
	);

	register_global_func(vm, 1, "load",     &native_load_str_str,      2, "bytecode", vm->Str, "func_name", vm->Str);
	_doc(vm, "", "Load compiled bytecode.");
	_doc(vm, "bytecode", "compile() result.");
	_doc(vm, "func_name", "Name of function to create. Used for backtraces and debugging purposes.");
	_doc(vm, "%RET", "Fun - function with no parameters that runs the loaded bytecode when called.");
	_doc_arr(vm, "%EX",
		"# From bootstrap.ngs, require() definition",
		"program_text = fetch(fname)",
		"program_bytecode = compile(program_text, fname)",
		"program_func = load(program_bytecode, \"require()d file: $fname\")",
		"ret = program_func()",
		NULL
	);

	register_global_func(vm, 1, "decode_json",&native_decode_json_str, 1, "s", vm->Str);
	_doc(vm, "", "Decode (parse) JSON.");
	_doc(vm, "%RET", "Any");
	_doc_arr(vm, "%EX",
		"decode_json('{\"a\": 1}')  # {a=1}",
		NULL
	);

	register_global_func(vm, 1, "encode_json",&native_encode_json_obj, 1, "obj", vm->Any);
	_doc(vm, "", "Encode JSON (serialize a data structure to JSON)");
	_doc(vm, "%RET", "Str");
	_doc_arr(vm, "%EX",
		"encode_json({\"a\": 1+1})  # The string { \"a\": 2 }",
		NULL
	);

	register_global_func(vm, 1, "Backtrace", &native_backtrace,         0);
	_doc(vm, "", "Backtrace constructor");

	register_global_func(vm, 1, "resolve_instruction_pointer", &native_resolve_instruction_pointer,       1, "ip", vm->Int);
	_doc(vm, "", "Resolves Instruction Pointer to source location");
	_doc(vm, "ip", "Result of calling Backtrace(). Backtrace().frames[0].ip for example.");
	_doc(vm, "%RET", "Hash with keys: file, first_line, first_column, last_line, last_column, ip");
	_doc_arr(vm, "%EX",
		"resolve_instruction_pointer(Backtrace().frames[0].ip)",
		"# {ip=4770, file=/etc/ngs/bootstrap.ngs, first_line=245, first_column=1, last_line=245, last_column=34}",
		NULL
	);

	register_global_func(vm, 1, "globals",  &native_globals,           0);
	_doc(vm, "", "Get all global variables as Hash");
	_doc(vm, "%RET", "Hash");
	_doc_arr(vm, "%EX",
		"globals().filterk(/^map/)  # {map=<MultiMethod with 7 method(s)>, mapv=...}",
		NULL
	);

	register_global_func(vm, 0, "c_gettimeofday",     &native_c_gettimeofday,         0);
	_doc(vm, "", "Wraps GETTIMEOFDAY(2).");
	_doc(vm, "%RET", "Arr[Int] - [tv_sec, tv_usec]");

	// TODO: check for errors, probably wrap in stdlib.
	register_global_func(vm, 0, "c_time",     &native_c_time,         0);
	_doc(vm, "", "Get time as the number of seconds since the Epoch, 1970-01-01 00:00:00 +0000 (UTC). Wraps TIME(2).");
	_doc(vm, "%RET", "Int");
	_doc_arr(vm, "%EX",
		"time()  # 1483780368",
		NULL
	);

	register_global_func(vm, 1, "c_gmtime",     &native_c_gmtime,         1, "timep", vm->Int);
	_doc(vm, "", "Call GMTIME_R(3)");
	_doc_arr(vm, "%EX",
		"ngs -pl 'c_gmtime(0).Hash().Strs()'",
		"tm_sec=0",
		"tm_min=0",
		"tm_hour=0",
		"tm_mday=1",
		"tm_mon=0",
		"tm_year=70",
		"tm_wday=4",
		"tm_yday=0",
		"tm_isdst=0",
		NULL
	);

	register_global_func(vm, 1, "c_localtime",  &native_c_localtime,      1, "timep", vm->Int);
	_doc(vm, "", "Call LOCALTIME(3)");
	_doc_arr(vm, "%EX",
		"ngs -pl 'c_gmtime(0).Hash().keys()'",
		"tm_sec",
		"tm_min",
		"tm_hour",
		"tm_mday",
		"tm_mon",
		"tm_year",
		"tm_wday",
		"tm_yday",
		"tm_isdst",
		NULL
	);

	register_global_func(vm, 0, "c_strftime",   &native_c_strftime,       2, "tm",    vm->c_tm, "format", vm->Str);
	_doc(vm, "", "Call STRFTIME(3)");

	register_global_func(vm, 1, "c_strptime",   &native_c_strptime,       2, "buf",   vm->Str,  "format", vm->Str);
	_doc(vm, "", "Call STRPTIME(3)");
	_doc(vm, "%RET", "Arr. [number_of_parsed_chars:Int, result:c_tm]");

	register_global_func(vm, 0, "c_mktime",     &native_c_mktime,         1, "tm",    vm->c_tm);
	_doc(vm, "", "Call MKTIME(3)");
	_doc(vm, "%RET", "Int - epoch time");


	// hash
	register_global_func(vm, 0, "in",       &native_in_any_hash,       2, "x",   vm->Any, "h", vm->Hash);
	_doc(vm, "", "Check key presence in a Hash");
	_doc(vm, "%RET", "Bool");
	_doc_arr(vm, "%EX",
		"\"a\" in {\"a\": 1}  # true",
		"\"b\" in {\"a\": 1}  # false",
		NULL
	);

	register_global_func(vm, 0, "hash",     &native_hash_any,          1, "x",   vm->Any);
	_doc(vm, "", "Calculate hash value. Same function that Hash uses internally. Currently Fowler-Noll-Vo (FNV) hash function.");
	_doc(vm, "%RET", "Int - unsigned 32 bit integer");
	_doc_arr(vm, "%EX",
		"hash(100)  # 100, Numbers are mapped to themselves.",
		"hash(\"A\")  # 84696414",
		"hash(\"AB\")  # 276232888",
		NULL
	);

	register_global_func(vm, 0, "keys",     &native_keys_hash,         1, "h",   vm->Hash);
	_doc(vm, "", "Get Hash keys as an array");
	_doc(vm, "%RET", "Arr");
	_doc_arr(vm, "%EX",
		"{\"a\": 1, \"b\": 2}.keys()  # ['a','b']",
		NULL
	);

	register_global_func(vm, 0, "values",   &native_values_hash,       1, "h",   vm->Hash);
	_doc(vm, "", "Get Hash values as an array");
	_doc(vm, "%RET", "Arr");
	_doc_arr(vm, "%EX",
		"{\"a\": 1, \"b\": 2}.values()  # [1,2]",
		NULL
	);

	register_global_func(vm, 0, "update",   &native_update_hash_hash,  2, "dst", vm->Hash, "src", vm->Hash);
	_doc(vm, "", "Update a Hash with key-value pairs from another Hash. For non destructive version use \"dst + src\".");
	_doc(vm, "%RET", "dst");
	_doc_arr(vm, "%EX",
		"{\"a\": 1, \"b\": 2}.update({\"b\": 10, \"c\": 20})  # {a=1, b=10, c=20}",
		NULL
	);

	register_global_func(vm, 0, "len",      &native_len,               1, "h",   vm->Hash);
	_doc(vm, "", "Get number of key-value pairs in a Hash");
	_doc(vm, "%RET", "Int");
	_doc_arr(vm, "%EX",
		"{\"a\": 1, \"b\": 2}.len()  # 2",
		NULL
	);

	register_global_func(vm, 0, "get",      &native_index_get_hash_any_any,    3, "h",   vm->Hash,"k", vm->Any, "dflt", vm->Any);
	_doc(vm, "", "Get hash value by key or dflt if it does not exist");
	_doc(vm, "%RET", "Any");
	_doc_arr(vm, "%EX",
		"h = {\"a\": 1}",
		"h.get(\"a\", 10)  # 1",
		"h.get(\"b\", 10)  # 10",
		NULL
	);

	register_global_func(vm, 1, "[]",       &native_index_get_hash_any,        2, "h",   vm->Hash,"k", vm->Any);
	_doc(vm, "", "Get hash value by key. Throws KeyNotFound.");
	_doc(vm, "%RET", "Any");
	_doc_arr(vm, "%EX",
		"h = {\"a\": 1}",
		"h[\"a\"]  # 1",
		"h[\"b\"]  # KeyNotFound exception thrown",
		NULL
	);

	register_global_func(vm, 0, "[]=",      &native_index_set_hash_any_any,    3, "h",   vm->Hash,"k", vm->Any, "v", vm->Any);
	_doc(vm, "", "Set hash value.");
	_doc(vm, "h", "Target hash");
	_doc(vm, "k", "Key");
	_doc(vm, "v", "Value");
	_doc(vm, "%RET", "v");
	_doc_arr(vm, "%EX",
		"h = {\"a\": 1}",
		"h[\"a\"] = 2",
		"h[\"a\"]  # 2",
		NULL
	);

	register_global_func(vm, 1, "del",      &native_index_del_hash_any,        2, "h",   vm->Hash,"k", vm->Any);
	_doc(vm, "", "Delete hash key. Throws KeyNotFound if k is not in h. WARNING: this method will probably be renamed to \"delete\" in future versions.");
	_doc(vm, "h", "Target hash");
	_doc(vm, "k", "Key");
	_doc(vm, "%RET", "h");
	_doc_arr(vm, "%EX",
		"h={\"a\": 1}; h.del(\"a\"); h  # {}",
		"h={}; h.del(\"a\"); # KeyNotFound exception",
		"h={\"a\": 1, \"b\": 2}; h.del(\"a\"); h  # {\"b\": 2}",
		NULL
	);

	register_global_func(vm, 0, "Hash",     &native_Hash_nti,                  1, "obj", vm->NormalTypeInstance);
	_doc(vm, "", "Get all fields and their values as key-value pairs in the resulting Hash.");
	_doc(vm, "%RET", "Hash");
	_doc_arr(vm, "%EX",
		"(1..10).Hash()  # Hash {start=1, end=10, step=1}",
		NULL
	);

	register_global_func(vm, 0, "Namespace", &native_Namespace, 0);
	_doc(vm, "", "Creates empty Namespace");
	_doc(vm, "%RET", "Namespace");

	register_global_func(vm, 1, "ll_hash_head",      &native_ll_hash_head,        1, "h",   vm->Hash);
	_doc(vm, "", "Low level. Do not use directly.");
	_doc(vm, "%RET", "LLHashEntry or null");

	register_global_func(vm, 1, "ll_hash_tail",      &native_ll_hash_tail,        1, "h",   vm->Hash);
	_doc(vm, "", "Low level. Do not use directly.");
	_doc(vm, "%RET", "LLHashEntry or null");

	register_global_func(vm, 1, "ll_hash_entry_key",      &native_ll_hash_entry_key,        1, "h",   vm->LLHashEntry);
	_doc(vm, "", "Low level. Do not use directly.");
	_doc(vm, "%RET", "Any");

	register_global_func(vm, 1, "ll_hash_entry_val",      &native_ll_hash_entry_val,        1, "h",   vm->LLHashEntry);
	_doc(vm, "", "Low level. Do not use directly.");
	_doc(vm, "%RET", "Any");

	register_global_func(vm, 1, "ll_hash_entry_next",      &native_ll_hash_entry_next,        1, "h",   vm->LLHashEntry);
	_doc(vm, "", "Low level. Do not use directly.");
	_doc(vm, "%RET", "LLHashEntry or null");

	register_global_func(vm, 0, "c_sysconf", &native_sysconf_int, 1, "name", vm->Int);

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
	set_global(vm, "method_not_found_handler", vm->method_not_found_handler = make_multimethod()); // There must be a catch-all in stdlib
	set_global(vm, "global_not_found_handler", vm->global_not_found_handler = make_multimethod());
	set_global(vm, "init", vm->init = make_multimethod());
	set_global(vm, "call", vm->call = make_multimethod());

	#define E(name) set_global(vm, "C_" #name, MAKE_INT(name))
	#include "errno.include"

	// awk '/^#define RTLD_/ {print $2}' /usr/include/x86_64-linux-gnu/bits/dlfcn.h ) > c_constants.txt
	E(RTLD_LAZY); E(RTLD_NOW); E(RTLD_NOLOAD); /* E(RTLD_DEEPBIND); E(RTLD_GLOBAL); E(RTLD_LOCAL); E(RTLD_NODELETE); */

	// --- man access(2) ---
	VALUE access = make_hash(8);
	#define A(name) set_hash_key(access, make_string(#name), MAKE_INT(name))
	A(F_OK); A(R_OK); A(W_OK); A(X_OK);
	#undef A
	set_global(vm, "ACCESS", access);

#ifdef HAVE_POLL_H
	// --- man poll(2) ---
	E(POLLIN); E(POLLPRI); E(POLLOUT); E(POLLERR); E(POLLHUP); E(POLLNVAL);
#endif

	// --- man 2 stat ---
	E(S_IFMT); E(S_IFSOCK); E(S_IFLNK); E(S_IFREG); E(S_IFBLK); E(S_IFDIR); E(S_IFCHR); E(S_IFIFO);

	E(S_ISUID); E(S_ISGID); E(S_ISVTX); E(S_IRWXU); E(S_IRUSR); E(S_IWUSR); E(S_IXUSR); E(S_IRWXG); E(S_IRGRP); E(S_IWGRP);
	E(S_IXGRP); E(S_IRWXO); E(S_IROTH); E(S_IWOTH); E(S_IXOTH);


	// --- pthread ---
	E(PTHREAD_MUTEX_RECURSIVE);


	// awk '/^#define PCRE/ && $3 {print "E("$2");"}' /usr/include/pcre.h | grep -v 'PCRE_UCHAR\|PCRE_SPTR' | sort | xargs -n5
	#pragma GCC diagnostic push
	// Silence clang unknown pragmas
	#pragma GCC diagnostic ignored "-Wunknown-pragmas"
	// Silence GCC unkown pragmas
	#pragma GCC diagnostic ignored "-Wpragmas"
	// Silence GCC 6 warnings (negative value)
	#pragma GCC diagnostic ignored "-Wshift-negative-value"
#include "pcre_constants.include"
	#pragma GCC diagnostic pop

	// --- sockets ---
	E(PF_UNIX);
	E(PF_INET);

	E(SOCK_STREAM);
	E(SOCK_DGRAM);
	E(SOCK_RAW);

	E(MSG_OOB);
	E(MSG_PEEK);
	E(MSG_WAITALL);

#undef E


	// based on procps/proc/sig.c - start
	VALUE signals = make_hash(64);

#define S(name) { \
		set_hash_key(signals, make_string(&(#name)[3]), MAKE_INT(name)); \
		set_hash_key(signals, MAKE_INT(name), make_string(&(#name)[3])); \
	}

	S(SIGABRT); S(SIGALRM); S(SIGBUS); S(SIGCHLD); S(SIGCONT);
#ifdef SIGEMT
	S(SIGEMT);
#endif
	S(SIGFPE); S(SIGHUP); S(SIGILL); S(SIGINT); S(SIGKILL); S(SIGPIPE); /* S(SIGPOLL); */ S(SIGPROF); /* S(SIGPWR); */ S(SIGQUIT); S(SIGSEGV);
#ifdef SIGSTKFLT
	S(SIGSTKFLT);
#endif
	S(SIGSTOP); S(SIGSYS); S(SIGTERM); S(SIGTRAP); S(SIGTSTP); S(SIGTTIN); S(SIGTTOU); S(SIGURG); S(SIGUSR1); S(SIGUSR2);
	S(SIGVTALRM); S(SIGWINCH); S(SIGXCPU); S(SIGXFSZ);
#undef S
	set_global(vm, "SIGNALS", signals);
	// based on procps/proc/sig.c - end

	{
		int d;
		(void)pcre_config(PCRE_CONFIG_NEWLINE, &d);
		set_global(vm, "PCRE_NEWLINE", MAKE_INT(d));
	}

#define FFI_TYPE(name) \
	vm->c_ ## name = make_ffi_type(&(name)); \
	set_global(vm, "c_" #name, vm->c_ ## name)

	// awk -F '[ ;]' '$1 == "FFI_EXTERN" {print "FFI_TYPE(" $3 ");"}' /usr/include/x86_64-linux-gnu/ffi.h
	FFI_TYPE(ffi_type_void);
	FFI_TYPE(ffi_type_uint8);
	FFI_TYPE(ffi_type_sint8);
	FFI_TYPE(ffi_type_uint16);
	FFI_TYPE(ffi_type_sint16);
	FFI_TYPE(ffi_type_uint32);
	FFI_TYPE(ffi_type_sint32);
	FFI_TYPE(ffi_type_uint64);
	FFI_TYPE(ffi_type_sint64);
	FFI_TYPE(ffi_type_float);
	FFI_TYPE(ffi_type_double);
	FFI_TYPE(ffi_type_pointer);
	FFI_TYPE(ffi_type_longdouble);
// #ifdef FFI_TARGET_HAS_COMPLEX_TYPE
// 	FFI_TYPE(ffi_type_complex_float);
// 	FFI_TYPE(ffi_type_complex_double);
// 	FFI_TYPE(ffi_type_complex_longdouble);
// #endif
	{
		ffi_type *t;
		t = NGS_MALLOC(sizeof(*t));
		assert(t);
		memcpy(t, &ffi_type_pointer, sizeof(*t));
		vm->c_ffi_type_string = make_ffi_type(t);
		set_global(vm, "c_ffi_type_string", vm->c_ffi_type_string);
	}

#undef FFI_TYPE

	// -- C_DEFS -- start --
	// TODO: migrate all other c definitions (above) into this namespace
	// https://github.com/ngs-lang/ngs/issues/113

	#define D(name) { set_hash_key(c_defs, make_string(#name), MAKE_INT(name)); }
	VALUE c_defs = make_namespace(128);

	#ifdef _SC_NPROCESSORS_ONLN
		D(_SC_NPROCESSORS_ONLN)
	#endif

	set_global(vm, "C_DEFS", c_defs);
	#undef D
	// -- C_DEFS -- end --

	set_global(vm, "INT_MIN", NGS_INT_MIN_VALUE);
	set_global(vm, "INT_MAX", NGS_INT_MAX_VALUE);
	set_global(vm, "RAND_MAX", MAKE_INT(NGS_RAND_MAX));


	// Documented in lib/stdlib.ngs
	VALUE gc = make_hash(4);
	set_hash_key(gc, make_string("enable"), make_func(vm, 0, "enable", &native_gc_enable, 0));
	set_hash_key(gc, make_string("disable"), make_func(vm, 0, "disable", &native_gc_disable, 0));
	set_hash_key(gc, make_string("get_parallel"), make_func(vm, 0, "get_parallel", &native_gc_get_parallel, 0));
	set_global(vm, "GC", gc);
}

void ctx_init(CTX *ctx) {
	ctx->stack_ptr = 0;
	ctx->frame_ptr = 0;
	memset(ctx->stack, 0, sizeof(ctx->stack));
	memset(ctx->frames, 0, sizeof(ctx->frames));
	// Probably not needed but conveys the message
	ctx->frames[ctx->frame_ptr].arr_callable = NULL;
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
				ENSURE_ARRAY_ROOM(vm->regions, vm->regions_allocated, vm->regions_len+1, 8);
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
					vm->bytecode = NGS_MALLOC_ATOMIC(len);
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
					gvi = get_global_index(vm, global_name, global_name_len);
					for(l=0; l<l_max; l++) {
						BYTECODE_GET(o, p, BYTECODE_GLOBALS_OFFSET);
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
						ENSURE_ARRAY_ROOM(region->files_names, region->files_names_allocated, region->files_names_len+1, 8);
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

#define SET_EXCEPTION_ARGS_KWARGS(exc, argc, argv) \
{ \
	int minus = (((argc) >= 2) && IS_KWARGS_MARKER((argv)[(argc)-1])) ? 2 : 0; \
	set_normal_type_instance_field(exc, make_string("args"), make_array_with_values((argc)-minus, (argv))); \
	if(minus) { \
		set_normal_type_instance_field(exc, make_string("kwargs"), (argv)[(argc)-2]); \
	} \
}

// XXX: Factor out to "define"s access to parameters. Coupling to this data structure is all over.
#define HAVE_KWARGS_MARKER ((argc >= 2) && IS_KWARGS_MARKER(argv[argc-1]))
METHOD_RESULT vm_call(VM *vm, CTX *ctx, VALUE *result, const VALUE callable, int argc, const VALUE *argv) {
	LOCAL_VAR_INDEX lvi;
	int i;
	METHOD_RESULT mr;
	VALUE *callable_items;

	if(IS_MULMETHOD(callable)) {
		int len = MULTIMETHOD_LEN(callable);
		DEEPER_FRAME.arr_callable = &callable;
		DEEPER_FRAME.arr_callable_idx = &i;
		for(i=len-1, callable_items = MULTIMETHOD_ITEMS(callable); i>=0; i--) {
			mr = vm_call(vm, ctx, result, callable_items[i], argc, argv);
			if((mr == METHOD_OK) || (mr == METHOD_EXCEPTION) || (mr == METHOD_IMPL_MISSING)) {
				DEEPER_FRAME.arr_callable = NULL;
				return mr;
			}
			assert(mr == METHOD_ARGS_MISMATCH);
		}
		DEEPER_FRAME.arr_callable = NULL;
		// --- method_not_found_handler() - start ---
		if(THIS_FRAME.do_call_method_not_found_handler) {
			// method_not_found_handler == [] when stdlib is not loaded (-E bootstrap switch / during basic tests)
			if(OBJ_LEN(vm->method_not_found_handler)) {
				VALUE new_argv;
				new_argv = make_array(argc+1);
				ARRAY_ITEMS(new_argv)[0] = callable;
				memcpy(&ARRAY_ITEMS(new_argv)[1], argv, sizeof(VALUE)*argc);
				THIS_FRAME.do_call_method_not_found_handler = 0;
				// last_ip should have been already set up before calling vm_call()
				mr = vm_call(vm, ctx, result, vm->method_not_found_handler, argc+1, ARRAY_ITEMS(new_argv));
				THIS_FRAME.do_call_method_not_found_handler = 1;
				if((mr == METHOD_OK) || (mr == METHOD_EXCEPTION)) {
					return mr;
				}
				assert(mr == METHOD_IMPL_MISSING);
			}
			// Either we called method_not_found_handler and it resulted METHOD_IMPL_MISSING
			// or we don't have method_not_found_handler
			VALUE exc;
			exc = make_normal_type_instance(vm->MethodNotFound);
			set_normal_type_instance_field(exc, make_string("message"), make_string("Failed to call method_not_found_handler()"));
			set_normal_type_instance_field(exc, make_string("callable"), callable);
			SET_EXCEPTION_ARGS_KWARGS(exc, argc, argv);
			THROW_EXCEPTION_INSTANCE(exc);
		}
		// --- method_not_found_handler() - end ---
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
		if(argc != NATIVE_METHOD_OBJ_N_REQ_PAR(callable)) {
			return METHOD_ARGS_MISMATCH;
		}
		for(lvi=0; lvi<NATIVE_METHOD_OBJ_N_REQ_PAR(callable); lvi++) {
			// TODO: make sure second argument is type durng closure creation
			if(!obj_is_of_type(vm, argv[lvi], NATIVE_METHOD_OBJ_PARAMS(callable)[lvi*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}
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
		int have_arr_splat = (CLOSURE_OBJ_PARAMS_FLAGS(callable) & PARAMS_FLAG_ARR_SPLAT) ? 1 : 0;
		int have_hash_splat = (CLOSURE_OBJ_PARAMS_FLAGS(callable) & PARAMS_FLAG_HASH_SPLAT) ? 1 : 0;
		int have_kwargs = HAVE_KWARGS_MARKER;
		int n_kwargs_used = 0;
		int i, j;
		VALUE kw, *params;
		HASH_OBJECT_ENTRY *e;
		VALUE named_arguments[MAX_ARGS];

		// TODO: handle (exception?) keyword arguments providing required parameter which was also given as positional argument

		if(have_kwargs) {
			kw = argv[argc-2];
			if(!IS_HASH(kw)) {
				VALUE exc;
				exc = make_normal_type_instance(vm->InternalError);
				set_normal_type_instance_field(exc, make_string("message"), make_string("Kwargs is not a hash"));
				THROW_EXCEPTION_INSTANCE(exc);
			}
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
			if(!obj_is_of_type(vm, argv[i], params[i*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}

		// Check the required arguments given as keyword arguments
		if(argc < n_params_required) {
			// We have some required parameters missing which might be in the keyword arguments
			assert(n_params_required < MAX_ARGS);
			for(i=argc; i<n_params_required; i++) {
				e = get_hash_key(kw, params[i*2 + 0]);
				if(!e) {
					// Required parameter is not in keyword arguments
					return METHOD_ARGS_MISMATCH;
				}
				if(!obj_is_of_type(vm, e->val, params[i*2+1])) {
					return METHOD_ARGS_MISMATCH;
				}
				named_arguments[i] = e->val;
				n_kwargs_used++;
			}
		}

		// Check optional parameters given as positional arguments
		j = MIN(argc, n_params_required + n_params_optional);
		for(i=n_params_required; i < j; i++) {
			// XXX temp - start
			if(!IS_NGS_TYPE(params[n_params_required*2 + (i-n_params_required)*3 + 1])) {
				VALUE exc;
				exc = make_normal_type_instance(vm->InvalidArgument);
				set_normal_type_instance_field(exc, make_string("message"), make_string("Parameter type expected"));
				set_normal_type_instance_field(exc, make_string("callable"), callable);
				set_normal_type_instance_field(exc, make_string("parameter_index"), MAKE_INT(i));
				set_normal_type_instance_field(exc, make_string("got"), params[n_params_required*2 + (i-n_params_required)*3 + 1]);
				THROW_EXCEPTION_INSTANCE(exc);
			}
			// XXX temp - end
			if(!obj_is_of_type(vm, argv[i], params[n_params_required*2 + (i-n_params_required)*3 + 1])) {
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
			if(!obj_is_of_type(vm, e->val, params[n_params_required*2 + (i-n_params_required)*3+1])) {
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
		ctx->frames[ctx->frame_ptr].do_call_method_not_found_handler = 1;
		ctx->frames[ctx->frame_ptr].do_call_call = 1;
		ctx->frames[ctx->frame_ptr].last_ip = 0;
		ctx->frame_ptr++;
		// MAX_FRAMES - 1 to allow DEEPER_FRAME to always work without checks
		if(ctx->frame_ptr >= MAX_FRAMES-1) {
			ctx->frame_ptr--;
			VALUE exc;
			exc = make_normal_type_instance(vm->StackDepthFail);
			set_normal_type_instance_field(exc, make_string("message"), make_string("Max stack depth reached"));
			THROW_EXCEPTION_INSTANCE(exc);
		}
		mr = vm_run(vm, ctx, CLOSURE_OBJ_IP(callable), result);
		ctx->frame_ptr--;
		return mr;
	}

	if(IS_NGS_TYPE(callable)) {
		return vm_call(vm, ctx, result, NGS_TYPE_CONSTRUCTORS(callable), argc, argv);
	}

	if(IS_NORMAL_TYPE_CONSTRUCTOR(callable)) {
		*result = make_normal_type_instance(NORMAL_TYPE_CONSTRUCTOR_TYPE(callable));
		if(obj_is_of_type(vm, *result, vm->Exception)) {
			set_normal_type_instance_field(*result, make_string("backtrace"), make_backtrace(vm, ctx));
		}
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
		THIS_FRAME.do_call_method_not_found_handler = 0;
		mr = vm_call(vm, ctx, &v, vm->init, argc+1, new_argv);
		THIS_FRAME.do_call_method_not_found_handler = 1;
		if(argc && (mr == METHOD_IMPL_MISSING)) {
			VALUE exc;
			exc = make_normal_type_instance(vm->MethodNotFound);
			set_normal_type_instance_field(exc, make_string("message"), make_string("Normal type constructor: init() not found"));
			set_normal_type_instance_field(exc, make_string("callable"), vm->init);
			SET_EXCEPTION_ARGS_KWARGS(exc, argc+1, new_argv);
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

	if(THIS_FRAME.do_call_call) {
		if(OBJ_LEN(vm->call)) {
			VALUE new_argv;
			new_argv = make_array(argc+1);
			ARRAY_ITEMS(new_argv)[0] = callable;
			memcpy(&ARRAY_ITEMS(new_argv)[1], argv, sizeof(VALUE)*argc);
			THIS_FRAME.do_call_call = 0;
			// last_ip should have been already set up before calling vm_call()
			mr = vm_call(vm, ctx, result, vm->call, argc+1, ARRAY_ITEMS(new_argv));
			THIS_FRAME.do_call_call = 1;
			if(mr == METHOD_OK) {
				return mr;
			}
			if(mr == METHOD_EXCEPTION) {
				VALUE r = MAKE_NULL;
				if(obj_is_of_type(vm, *result, vm->MethodNotFound)) {
					get_normal_type_instance_field(*result, make_string("callable"), &r);
					if(r.ptr == vm->call.ptr) {
						// Don't know how to call
						mr = METHOD_IMPL_MISSING;
					} else {
						return mr;
					}
				} else {
					return mr;
				}
			}
			assert(mr == METHOD_IMPL_MISSING);
		}
		// Either we called call and it resulted METHOD_IMPL_MISSING
		// or we don't have call
	}

	VALUE exc;
	exc = make_normal_type_instance(vm->DontKnowHowToCall);
	set_normal_type_instance_field(exc, make_string("message"), make_string("No matching call() found"));
	set_normal_type_instance_field(exc, make_string("callable"), callable);
	SET_EXCEPTION_ARGS_KWARGS(exc, argc, argv);
	THROW_EXCEPTION_INSTANCE(exc);

}
#undef HAVE_KWARGS_MARKER

METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result) {
	VALUE v, callable, command;
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
			dump(stderr, ctx->stack[j-1]);
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
		case OP_PUSH_INT32:
							{
								// WARNING! Highest bits get lost! Works with TAG_BITS highest bits unset!
								int32_t i = *(int32_t *) &vm->bytecode[ip];
								ip += sizeof(i);
								PUSH(MAKE_INT(i));
								goto main_loop;
							}
		case OP_PUSH_INT64:
							{
								int64_t i = *(int64_t *) &vm->bytecode[ip];
								ip += sizeof(i);
								PUSH(MAKE_INT(i));
								goto main_loop;
							}
		case OP_PUSH_REAL:
							PUSH(make_real(*(NGS_REAL *) &vm->bytecode[ip]));
							ip += sizeof(NGS_REAL);
							goto main_loop;
		case OP_PUSH_L8_STR:
							// Arg: LEN8 + string
							v = make_string_of_len(&(vm->bytecode[ip+sizeof(unsigned char)]), (unsigned char) vm->bytecode[ip]);
							ip += sizeof(unsigned char) + (unsigned char) vm->bytecode[ip];
							PUSH(v);
							goto main_loop;
		case OP_PUSH_L32_STR:
							// Arg: LEN32 + string
							v = make_string_of_len(&(vm->bytecode[ip+sizeof(uint32_t)]), *(uint32_t *) &vm->bytecode[ip]);
							ip += sizeof(uint32_t) + *(uint32_t *) &vm->bytecode[ip];
							PUSH(v);
							goto main_loop;
#define OP_MAKE_STR_(type_name) \
	EXPECT_STACK_DEPTH(1); \
	v = make_normal_type_instance(vm->type_name); \
	set_normal_type_instance_field(v, make_string("val"), TOP); \
	TOP = v; \
	goto main_loop;
		case OP_MAKE_STR_IMM:
							OP_MAKE_STR_(NgsStrCompImm);
		case OP_MAKE_STR_EXP:
							OP_MAKE_STR_(NgsStrCompExp);
		case OP_MAKE_STR_SPLAT_EXP:
							OP_MAKE_STR_(NgsStrCompSplatExp);
#undef OP_MAKE_STR_
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
							// Effect: bytecode[ip+offset] <- n
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
							if(IS_UNDEF(GLOBALS[gvi])) {
								THIS_FRAME.last_ip = ip;

								THIS_FRAME.do_call_method_not_found_handler = 0;
								// last_ip should have been already set up before calling vm_call()
								v = make_string(vm->globals_names[gvi]);
								mr = vm_call(vm, ctx, result, vm->global_not_found_handler, 1, &v);
								THIS_FRAME.do_call_method_not_found_handler = 1;
								if(IS_UNDEF(GLOBALS[gvi]) || mr == METHOD_EXCEPTION) {
									VALUE exc;
									exc = make_normal_type_instance(vm->GlobalNotFound);
									set_normal_type_instance_field(exc, make_string("name"), make_string(vm->globals_names[gvi]));
									set_normal_type_instance_field(exc, make_string("index"), MAKE_INT(gvi));
									set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
									if(mr == METHOD_EXCEPTION) {
										set_normal_type_instance_field(exc, make_string("cause"), *result);
									} else {
										set_normal_type_instance_field(exc, make_string("global_not_found_handler_ret_val"), *result);
										if (mr == METHOD_IMPL_MISSING) {
											set_normal_type_instance_field(exc, make_string("extra_info"), make_string("Additionally, no appropriate global_not_found_handler() found"));
										} else {
											set_normal_type_instance_field(exc, make_string("extra_info"), make_string("Additionally, global_not_found_handler() failed to provide the global"));
										}
									}
									*result = exc;
									goto exception;
								}
							}
							// dump_titled(stderr, "FETCH_GLOBAL", GLOBALS[gvi]);
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
							if(IS_UNDEF(LOCALS[lvi])) {
								VALUE exc;
								exc = make_normal_type_instance(vm->UndefinedLocalVar);
								set_normal_type_instance_field(exc, make_string("name"), CLOSURE_OBJ_LOCALS(THIS_FRAME_CLOSURE)[lvi]);
								set_normal_type_instance_field(exc, make_string("index"), MAKE_INT(lvi));
								set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
								*result = exc;
								goto exception;
							}
							PUSH(LOCALS[lvi]);
							goto main_loop;
		case OP_STORE_LOCAL:
							ARG_LVI;
							POP(LOCALS[lvi]);
							goto main_loop;
		case OP_CALL:
                            call:
							// TODO: print arguments of failed call, not just the callable
							// In (current): ... result_placeholder (null), arg1, ..., argN, argc, callable
							// Out: ... result
							EXPECT_STACK_DEPTH(2);
							POP_NOCHECK(callable);
							POP_NOCHECK(v); // number of arguments
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-GET_INT(v)-1], callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							if(mr == METHOD_EXCEPTION) {
								*result = ctx->stack[ctx->stack_ptr-GET_INT(v)-1];
								goto exception;
							}
							if(mr == METHOD_ARGS_MISMATCH) {
								*result = make_normal_type_instance(vm->ArgsMismatch);
								set_normal_type_instance_field(*result, make_string("message"), make_string("Arguments did not match"));
								set_normal_type_instance_field(*result, make_string("callable"), callable);
								SET_EXCEPTION_ARGS_KWARGS(*result, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
								set_normal_type_instance_field(*result, make_string("backtrace"), make_backtrace(vm, ctx));
								goto exception;
							}
							assert(mr == METHOD_OK);
							REMOVE_TOP_N(GET_INT(v));
							goto main_loop;
		case OP_CALL_EXC:
							// Calls exception handler, METHOD_IMPL_MISSING means we should re-throw the exception
							EXPECT_STACK_DEPTH(2);
							POP_NOCHECK(callable);
							POP_NOCHECK(v); // number of arguments
							THIS_FRAME.do_call_method_not_found_handler = 0;
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-GET_INT(v)-1], callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							THIS_FRAME.do_call_method_not_found_handler = 1;
							if(mr == METHOD_EXCEPTION) {
								// TODO: special handling? Exception during exception handling.
								*result = ctx->stack[ctx->stack_ptr-GET_INT(v)-1];
								goto exception;
							}
							if(mr == METHOD_IMPL_MISSING) {
								// Stack: null (for vm_call result which did not happen), exception
								POP(*result);
								goto exception_return;
							}
							assert(mr == METHOD_OK);
							REMOVE_TOP_N(GET_INT(v));
							goto main_loop;
		case OP_CALL_ARR:
							// In (current): ... result_placeholder (null), argv, callable
							// Out: ... result
							POP(callable);
							THIS_FRAME.last_ip = ip;
							mr = vm_call(vm, ctx, &ctx->stack[ctx->stack_ptr-2], callable, OBJ_LEN(ctx->stack[ctx->stack_ptr-1]), ARRAY_ITEMS(ctx->stack[ctx->stack_ptr-1]));
							if(mr == METHOD_EXCEPTION) {
								*result = ctx->stack[ctx->stack_ptr-2];
								goto exception;
							}
							if(mr != METHOD_OK) {
								if(mr == METHOD_ARGS_MISMATCH) {
									VALUE exc;
									exc = make_normal_type_instance(vm->MethodNotFound);
									set_normal_type_instance_field(exc, make_string("callable"), callable);
									SET_EXCEPTION_ARGS_KWARGS(exc, OBJ_LEN(ctx->stack[ctx->stack_ptr-1]), ARRAY_ITEMS(ctx->stack[ctx->stack_ptr-1]));
									set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
									*result = exc;
									goto exception;
								}
								dump_titled(stderr, "Failed argument array", ctx->stack[ctx->stack_ptr-1]);
								dump_titled(stderr, "Failed callable / 3", callable);
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
							ctx->stack_ptr -= (n_params_required + ADDITIONAL_PARAMS_COUNT)*2 + n_params_optional*3;
							v = make_closure_obj(
									ip+jo,
									n_locals, n_params_required, n_params_optional, n_uplevels, params_flags,
									&ctx->stack[ctx->stack_ptr],
									&ctx->stack[ctx->stack_ptr - n_locals]
							);
							ctx->stack_ptr -= n_locals;
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
							{
								int i;
								POP(v);
								string_components_count = GET_INT(v);
								EXPECT_STACK_DEPTH(string_components_count);
								for(i=0;i<string_components_count;i++) {
									if(!IS_STRING(ctx->stack[ctx->stack_ptr-string_components_count+i])) {
										VALUE exc;
										exc = make_normal_type_instance(vm->InvalidArgument);
										set_normal_type_instance_field(exc, make_string("message"), make_string("String interpolation requires all components to be strings"));
										set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
										*result = exc;
										goto exception;
									}
								}
								v = join_strings(string_components_count, &(ctx->stack[ctx->stack_ptr-string_components_count]));
								ctx->stack_ptr -= string_components_count;
								PUSH_NOCHECK(v);
								goto main_loop;
							}
		case OP_PUSH_EMPTY_STR:
							PUSH(make_var_len_obj(T_STR, 1, 0));
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
								GLOBALS[gvi] = make_multimethod_with_value(TOP);
							} else {
								// printf("GVI %d %s\n", gvi, vm->globals_names[gvi]);
								PUSH_METHOD(GLOBALS[gvi], TOP);
							}
							goto main_loop;
		case OP_DEF_LOCAL_FUNC:
							// Arg: lvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							ARG_LVI;
							if(IS_UNDEF(LOCALS[lvi])) {
								LOCALS[lvi] = make_multimethod_with_value(TOP);
							} else {
								PUSH_METHOD(LOCALS[lvi], TOP);
							}
							goto main_loop;
		case OP_FETCH_UPVAR:
#ifdef DO_NGS_DEBUG
							assert(ctx->frame_ptr);
#endif
							ARG_UVI;
							ARG_LVI;
							// printf("uvi=%d lvi=%d\n", uvi, lvi);

							if(IS_UNDEF(UPLEVELS[uvi][lvi])) {
								VALUE exc;
								exc = make_normal_type_instance(vm->UndefinedUpVar);
								// TODO: variable name
								// set_normal_type_instance_field(exc, make_string("name"), CLOSURE_OBJ_LOCALS(THIS_FRAME_CLOSURE)[lvi]);
								set_normal_type_instance_field(exc, make_string("level"), MAKE_INT(uvi));
								set_normal_type_instance_field(exc, make_string("index"), MAKE_INT(lvi));
								set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
								*result = exc;
								goto exception;
							}

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
								UPLEVELS[uvi][lvi] = make_multimethod_with_value(TOP);
							} else {
								PUSH_METHOD(UPLEVELS[uvi][lvi], TOP);
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
		case OP_ARR_APPEND2:
							EXPECT_STACK_DEPTH(3);
							array_push(ctx->stack[ctx->stack_ptr-3], ctx->stack[ctx->stack_ptr-2]);
							array_push(ctx->stack[ctx->stack_ptr-3], ctx->stack[ctx->stack_ptr-1]);
							REMOVE_TOP_N_NOCHECK(2);
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
							POP(v);  // Guard failed so the value is not needed
							assert(saved_stack_ptr == ctx->stack_ptr);
							return METHOD_ARGS_MISMATCH;
		case OP_TRY_START:
							assert(THIS_FRAME.try_info_ptr < MAX_TRIES_PER_FRAME);
							ARG(jo, JUMP_OFFSET);
							THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].catch_ip = ip + jo;
							THIS_FRAME.try_info[THIS_FRAME.try_info_ptr].saved_stack_ptr = ctx->stack_ptr;
							THIS_FRAME.try_info_ptr++;
							goto main_loop;
		case OP_TRY_END:
							assert(THIS_FRAME.try_info_ptr);
							THIS_FRAME.try_info_ptr--;
							goto do_jump;
		case OP_ARR_REVERSE:
							EXPECT_STACK_DEPTH(1);
							array_reverse(TOP);
							goto main_loop;
		case OP_THROW:
							POP(*result);
							// The place that Exception is created is the right place to set backtrace, not where it is thrown
							// so not setting *result backtrace property here.
							goto exception;
		case OP_MAKE_CMDS_PIPELINE:
							EXPECT_STACK_DEPTH(3);
							command = make_normal_type_instance(vm->CommandsPipeline);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("options"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("pipes"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("commands"), v);
							PUSH_NOCHECK(command);
							goto main_loop;
		case OP_MAKE_CMDS_PIPE:
							EXPECT_STACK_DEPTH(2);
							command = make_normal_type_instance(vm->CommandsPipe);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("options"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("name"), v);
							PUSH_NOCHECK(command);
							goto main_loop;
		case OP_MAKE_CMD:
							EXPECT_STACK_DEPTH(3);
							command = make_normal_type_instance(vm->Command);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("options"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("redirects"), v);
							POP_NOCHECK(v);
							set_normal_type_instance_field(command, make_string("argv"), v);
							PUSH_NOCHECK(command);
							goto main_loop;
		case OP_SET_CLOSURE_NAME:
							EXPECT_STACK_DEPTH(1);
							assert(IS_CLOSURE(TOP));
							v = make_string_of_len(&vm->bytecode[ip+1], vm->bytecode[ip]);
							ip += 1 + vm->bytecode[ip];
							// CLOSURE_OBJ_NAME(TOP) = v;
							if(!IS_HASH(OBJ_META(TOP))) {
								goto main_loop;
							}
							set_hash_key(OBJ_META(TOP), make_string("name"), v);
							goto main_loop;
		case OP_SET_CLOSURE_DOC:
							EXPECT_STACK_DEPTH(2);
							assert(IS_CLOSURE(SECOND));
							if(!IS_HASH(OBJ_META(SECOND))) {
								goto main_loop;
							}
							set_hash_key(OBJ_META(SECOND), make_string("doc"), FIRST);
							REMOVE_TOP_NOCHECK;
							goto main_loop;
		case OP_SET_CLOSURE_NS:
							EXPECT_STACK_DEPTH(2);
							assert(IS_CLOSURE(SECOND));
							if(!IS_HASH(OBJ_META(SECOND))) {
								goto main_loop;
							}
							set_hash_key(OBJ_META(SECOND), make_string("ns"), FIRST);
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
							assert(IS_HASH(TOP));
							PUSH(MAKE_KWARGS_MARKER);
							goto main_loop;
		case OP_MAKE_REDIR:
                            // In: (null), fd, marker, datum
                            // OP_CALL - In (current): ... result_placeholder (null), arg1, ..., argN, argc, callable
							EXPECT_STACK_DEPTH(4);
                            PUSH(MAKE_INT(3));
                            PUSH(vm->CommandRedir);
                            goto call;
		case OP_SUPER:
							if(THIS_FRAME.arr_callable) {
								assert(IS_MULMETHOD(*THIS_FRAME.arr_callable));
								// PUSH(make_array_with_values(*THIS_FRAME.arr_callable_idx, OBJ_DATA_PTR(*THIS_FRAME.arr_callable)));
								v = make_multimethod();
								MULTIMETHOD_METHODS(v) = make_array_with_values(*THIS_FRAME.arr_callable_idx, MULTIMETHOD_ITEMS(*THIS_FRAME.arr_callable));
								PUSH(v);
								goto main_loop;
							} else {
								VALUE exc;
								// TODO: better exception type than MethodNotFound
								exc = make_normal_type_instance(vm->MethodNotFound);
								set_normal_type_instance_field(exc, make_string("message"), make_string("Using super where callable is not an array"));
								set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
								*result = exc;
								goto exception;
							}
		case OP_MAKE_MULTIMETHOD:
							PUSH(make_multimethod());
							goto main_loop;
		case OP_MULTIMETHOD_APPEND:
							EXPECT_STACK_DEPTH(2);
							push_multimethod_method(ctx->stack[ctx->stack_ptr-2], ctx->stack[ctx->stack_ptr-1]);
							REMOVE_TOP_NOCHECK;
							goto main_loop;
		case OP_MULTIMETHOD_REVERSE:
							// XXX: leaked abstraction
							EXPECT_STACK_DEPTH(1);
							array_reverse(MULTIMETHOD_METHODS(TOP));
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
							assert(0 == "Unknown opcode");
	}

end_main_loop:
	return METHOD_OK;

stack_overflow_exception:
	{
		VALUE exc;
		exc = make_normal_type_instance(vm->StackOverflow);
		set_normal_type_instance_field(exc, make_string("message"), make_string("Stack Overflow"));
		set_normal_type_instance_field(exc, make_string("backtrace"), make_backtrace(vm, ctx));
		*result = exc;
		// goto exception
	}

exception:
	// *result is the exception
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
	h->data = NGS_MALLOC_ATOMIC(len);
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

	if(memcmp(p, BYTECODE_SIGNATURE, strlen(BYTECODE_SIGNATURE)) != 0) {
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
