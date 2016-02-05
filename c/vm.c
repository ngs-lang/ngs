#include <assert.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdio.h>

// OPEN(2), LSEEK(2)
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

// READ(2), LSEEK(2)
#include <unistd.h>

// BCMP(3)
#include <strings.h>

#include "ngs.h"
#include "vm.h"

extern char **environ;

char *opcodes_names[] = {
	/*  0 */ "HALT",
	/*  1 */ "PUSH_NULL",
	/*  2 */ "PUSH_FALSE",
	/*  3 */ "PUSH_TRUE",
	/*  4 */ "PUSH_UNDEF",
	/*  5 */ "PUSH_INT",
	/*  6 */ "PUSH_L_STR",
	/*  7 */ "DUP",
	/*  8 */ "POP",
	/*  9 */ "RESOLVE_GLOBAL",
	/* 10 */ "PATCH",
	/* 11 */ "INIT_DONE",
	/* 12 */ "FETCH_GLOBAL",
	/* 13 */ "STORE_GLOBAL",
	/* 14 */ "FETCH_LOCAL",
	/* 15 */ "STORE_LOCAL",
	/* 16 */ "CALL",
	/* 17 */ "RET",
	/* 18 */ "JMP",
	/* 19 */ "JMP_TRUE",
	/* 20 */ "JMP_FALSE",
	/* 21 */ "MAKE_ARR",
	/* 22 */ "MAKE_CLOSURE",
	/* 23 */ "TO_STR",
	/* 24 */ "MAKE_STR",
	/* 25 */ "PUSH_EMPTY_STR",
	/* 26 */ "GLOBAL_DEF_P",
	/* 27 */ "LOCAL_DEF_P",
	/* 28 */ "DEF_GLOBAL_FUNC",
	/* 29 */ "DEF_LOCAL_FUNC",
	/* 30 */ "FETCH_UPVAR",
	/* 31 */ "STORE_UPVAR",
	/* 32 */ "UPVAR_DEF_P",
	/* 33 */ "DEF_UPVAR_FUNC",
	/* 34 */ "MAKE_HASH",
	/* 35 */ "TO_BOOL",
};


#define PUSH(v) assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr++] = v
#define POP(dst) assert(ctx->stack_ptr); ctx->stack_ptr--; dst = ctx->stack[ctx->stack_ptr]
#define TOP (ctx->stack[ctx->stack_ptr-1])
#define DUP assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr] = ctx->stack[ctx->stack_ptr-1]; ctx->stack_ptr++;
#define REMOVE_TOP assert(ctx->stack_ptr); ctx->stack_ptr--; 
#define PUSH_NULL PUSH((VALUE){.num=V_NULL})
#define GLOBALS (vm->globals)
#define LOCALS (ctx->frames[ctx->frame_ptr-1].locals)
#define THIS_FRAME_CLOSURE (ctx->frames[ctx->frame_ptr-1].closure)
#define UPLEVELS CLOSURE_OBJ_UPLEVELS(THIS_FRAME_CLOSURE)
#define ARG(name, type) name = *(type *) &vm->bytecode[ip]; ip += sizeof(type);
#define ARG_LVI ARG(lvi, LOCAL_VAR_INDEX);
#define ARG_GVI ARG(gvi, GLOBAL_VAR_INDEX);
#define ARG_UVI ARG(uvi, UPVAR_INDEX);

#define METHOD_PARAMS (VALUE *argv, VALUE *result)
#define METHOD_RETURN(v) { *result = (v); return METHOD_OK; }

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

#define ARG_LEN(n) OBJ_LEN(argv[n])
#define ARG_DATA_PTR(n) OBJ_DATA_PTR(argv[n])

INT_METHOD(plus, +);
INT_METHOD(minus, -);
INT_METHOD(mul, *);
INT_METHOD(div, /);
INT_METHOD(mod, %);
INT_CMP_METHOD(less, <);
INT_CMP_METHOD(less_eq, <=);
INT_CMP_METHOD(greater, >);
INT_CMP_METHOD(greater_eq, >=);
INT_CMP_METHOD(eq, ==);

METHOD_RESULT native_dump_any METHOD_PARAMS {
	dump(argv[0]);
	SET_NULL(*result);
	return METHOD_OK;
}

METHOD_RESULT native_echo_str METHOD_PARAMS {
	SET_INT(*result, printf("%s\n", obj_to_cstring(argv[0])));
	return METHOD_OK;
}

METHOD_RESULT native_plus_arr_arr METHOD_PARAMS {
	*result = make_array(ARG_LEN(0) + ARG_LEN(1));
	memcpy(ARRAY_ITEMS(*result)+0, ARG_DATA_PTR(0), sizeof(VALUE)*ARG_LEN(0));
	memcpy(ARRAY_ITEMS(*result)+ARG_LEN(0), OBJ_DATA_PTR(argv[1]), sizeof(VALUE)*ARG_LEN(1));
	return METHOD_OK;
}

METHOD_RESULT native_push_arr_any METHOD_PARAMS { array_push(argv[0], argv[1]); METHOD_RETURN(argv[0]); }

METHOD_RESULT native_shift_arr METHOD_PARAMS { METHOD_RETURN(array_shift(argv[0])); }

METHOD_RESULT native_shift_arr_any METHOD_PARAMS {
	if(!OBJ_LEN(argv[0])) {
		METHOD_RETURN(argv[1]);
	}
	METHOD_RETURN(array_shift(argv[0]));
}

METHOD_RESULT native_index_get_arr_int METHOD_PARAMS {
	int idx, len;
	idx = GET_INT(argv[1]);
	assert(idx>=0);
	len = OBJ_LEN(argv[0]);
	assert(idx<len); // TODO: Throw exception
	*result = ARRAY_ITEMS(argv[0])[idx];
	return METHOD_OK;
}


METHOD_RESULT native_Str_int METHOD_PARAMS {
	char s[MAX_INT_TO_STR_LEN];
	size_t len;
	len = snprintf(s, sizeof(s), "%" PRIiPTR, GET_INT(argv[0]));
	assert(len<sizeof(s)); // Or we might haver truncated represnetation
	*result = make_string(s);
	return METHOD_OK;
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
	if(IS_STRING(argv[0]) || IS_ARRAY(argv[0])) METHOD_RETURN(MAKE_BOOL(OBJ_LEN(argv[0])))
	return METHOD_ARGS_MISMATCH;
}

METHOD_RESULT native_not_any METHOD_PARAMS {
	if(!IS_BOOL(argv[0])) {
		assert(0=="not() on non-booleans is not implemented yet");
		// TODO: Call Bool() on the value, then continue with not() on the returned value
		// ...
		assert(IS_BOOL(argv[0]));
	}
	METHOD_RETURN(GET_INVERTED_BOOL(argv[0]));
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

METHOD_RESULT native_len METHOD_PARAMS {
	*result = MAKE_INT(OBJ_LEN(argv[0]));
	return METHOD_OK;
}

METHOD_RESULT native_index_get_hash_any METHOD_PARAMS {
	HASH_OBJECT_ENTRY *e;
	e = get_hash_key(argv[0], argv[1]);
	if(!e) {
		assert(0=="Don't know how to throw KeyNotFound exception yet");
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
// TODO: a  way to return errno. Exception maybe?
METHOD_RESULT native_c_open_str_str METHOD_PARAMS {
	const char *pathname = obj_to_cstring(argv[0]);
	const char *flags_str = obj_to_cstring(argv[1]);
	int flags = 0;
	if(!flags && !strcmp(flags_str, "r"))  { flags = O_RDONLY; }
	if(!flags && !strcmp(flags_str, "w"))  { flags = O_WRONLY; }
	if(!flags && !strcmp(flags_str, "rw")) { flags = O_RDWR; }
	SET_INT(*result, open(pathname, flags));
	return METHOD_OK;
}

// READ(2)
// TODO: error handling support
METHOD_RESULT native_c_read_int_int METHOD_PARAMS {
	// Params: fd, count
	char *buf;
	size_t count = GET_INT(argv[1]);
	ssize_t ret;
	assert(count <= SSIZE_MAX);
	buf = NGS_MALLOC_ATOMIC(count);
	assert(buf);
	ret = read(GET_INT(argv[0]), buf, count);

	if(ret < 0) {
		SET_INT(*result, ret);
		return METHOD_OK;
	}

	*result = make_string_of_len(buf, ret);
	return METHOD_OK;
}

METHOD_RESULT native_c_lseek_int_int_str METHOD_PARAMS {
	off_t offset;
	const char *whence_str = obj_to_cstring(argv[2]);
	int whence = 0;
	if(!strcmp(whence_str, "set")) {
		whence = SEEK_SET;
	} else {
		if(!strcmp(whence_str, "cur")) {
			whence = SEEK_CUR;
		} else {
			if(!strcmp(whence_str, "end")) {
				whence = SEEK_END;
			} else {
				// TODO: Exception
				assert(0 == "c_lseek(): Invalid whence");
			}
		}
	}
	offset = lseek(GET_INT(argv[0]), GET_INT(argv[1]), whence);
	METHOD_RETURN(MAKE_INT(offset));
}

METHOD_RESULT native_eq_str_str METHOD_PARAMS {
	size_t len;
	if(OBJ_LEN(argv[0]) != OBJ_LEN(argv[1])) { METHOD_RETURN(MAKE_BOOL(0)); }
	if(OBJ_DATA_PTR(argv[0]) == OBJ_DATA_PTR(argv[1])) { METHOD_RETURN(MAKE_BOOL(1)); }
	len = OBJ_LEN(argv[0]);
	METHOD_RETURN(MAKE_BOOL(!bcmp(OBJ_DATA_PTR(argv[0]), OBJ_DATA_PTR(argv[1]), len)));
}

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
	GLOBALS[var->index].num = V_UNDEF;
	vm->globals_names[var->index] = var->name;
	DEBUG_VM_RUN("leaving get_global_index() status=new vm=%p name=%.*s -> index=" GLOBAL_VAR_INDEX_FMT "\n", vm, (int)name_len, name, var->index);
	return var->index;
}

void register_global_func(VM *vm, char *name, void *func_ptr, LOCAL_VAR_INDEX argc, ...) {
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

void set_global(VM *vm, const char *name, VALUE v) {
	size_t index;
	index = get_global_index(vm, name, strlen(name));
	GLOBALS[index] = v;
}

NGS_TYPE *register_builtin_type(VM *vm, const char *name, NATIVE_TYPE_ID native_type_id) {
	size_t index;
	NGS_TYPE *t;
	t = NGS_MALLOC(sizeof(*t));
	assert(t);
	t->base.type.num = T_TYPE;
	t->base.val.ptr = NULL; /* unused */
	t->name = make_string(name);
	t->constructors = make_array(0);
	t->meta = make_array(0); /* unused for now */
	t->native_type_id = native_type_id;


	index = get_global_index(vm, name, strlen(name));
	assert(IS_UNDEF(GLOBALS[index]));
	SET_OBJ(GLOBALS[index], t);
	return t;
}

void vm_init(VM *vm, int argc, char **argv) {
	char **env, *equal_sign;
	VALUE env_hash, k, v;
	VALUE argv_array;
	int i;
	vm->bytecode = NULL;
	vm->globals_indexes = NULL; // UT_hash_table
	vm->globals_len = 0;
	vm->globals = NGS_MALLOC(sizeof(*(vm->globals)) * MAX_GLOBALS);
	vm->globals_names = NGS_MALLOC(sizeof(char *) * MAX_GLOBALS);
	// Keep global functions registration in order.
	// This way the compiler can use globals_indexes as the beginning of
	// it's symbol table for globals.
	vm->Null = register_builtin_type(vm, "Null", T_NULL);
	vm->Bool = register_builtin_type(vm, "Bool", T_BOOL);
	vm->Int  = register_builtin_type(vm, "Int",  T_INT);
	vm->Str  = register_builtin_type(vm, "Str",  T_STR);
	vm->Arr  = register_builtin_type(vm, "Arr",  T_ARR);
	vm->Fun  = register_builtin_type(vm, "Fun",  T_FUN);
	vm->Any  = register_builtin_type(vm, "Any",  T_ANY);
	vm->Seq  = register_builtin_type(vm, "Seq",  T_SEQ);
	vm->Type = register_builtin_type(vm, "Type", T_TYPE);
	vm->Hash = register_builtin_type(vm, "Hash", T_HASH);
	vm->CLib = register_builtin_type(vm, "CLib", T_CLIB);
	vm->CSym = register_builtin_type(vm, "CSym", T_CSYM);

	// CLib and c calls
	register_global_func(vm, "CLib",     &native_CLib_str,          1, "name",   vm->Str);
	register_global_func(vm, "in",       &native_in_str_clib,       2, "symbol", vm->Str, "lib", vm->CLib);
	register_global_func(vm, "[]",       &native_index_get_clib_str,2, "lib",    vm->CLib,"symbol", vm->Str);

	// low level file operations
	register_global_func(vm, "c_open",   &native_c_open_str_str,    2, "pathname", vm->Str, "flags", vm->Str);
	register_global_func(vm, "c_read",   &native_c_read_int_int,    2, "fd",       vm->Int, "count", vm->Int);
	register_global_func(vm, "c_lseek",  &native_c_lseek_int_int_str,3,"fd",       vm->Int, "offset", vm->Int, "whence", vm->Str);

	// array
	register_global_func(vm, "+",        &native_plus_arr_arr,      2, "a",   vm->Arr, "b", vm->Arr);
	register_global_func(vm, "push",     &native_push_arr_any,      2, "arr", vm->Arr, "v", vm->Any);
	register_global_func(vm, "shift",    &native_shift_arr,         1, "arr", vm->Arr);
	register_global_func(vm, "shift",    &native_shift_arr_any,     2, "arr", vm->Arr, "dflt", vm->Any);
	register_global_func(vm, "len",      &native_len,               1, "arr", vm->Arr);
	register_global_func(vm, "[]",       &native_index_get_arr_int, 2, "arr", vm->Arr, "idx", vm->Int);

	// string
	// TODO: other string comparison operators
	register_global_func(vm, "==",       &native_eq_str_str,        2, "a",   vm->Str, "b", vm->Str);

	// int
	register_global_func(vm, "+",        &native_plus_int_int,      2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "*",        &native_mul_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "/",        &native_div_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "%",        &native_mod_int_int,       2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "-",        &native_minus_int_int,     2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "<",        &native_less_int_int,      2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "<=",       &native_less_eq_int_int,   2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, ">",        &native_greater_int_int,   2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, ">=",       &native_greater_eq_int_int,2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "==",       &native_eq_int_int,        2, "a",   vm->Int, "b", vm->Int);

	// misc
	register_global_func(vm, "dump",     &native_dump_any,          1, "obj", vm->Any);
	register_global_func(vm, "echo",     &native_echo_str,          1, "s",   vm->Str);
	register_global_func(vm, "Bool",     &native_Bool_any,          1, "x",   vm->Any);
	register_global_func(vm, "Str",      &native_Str_int,           1, "n",   vm->Int);
	register_global_func(vm, "is",       &native_is_any_type,       2, "obj", vm->Any, "t", vm->Type);
	register_global_func(vm, "not",      &native_not_any,           1, "x",   vm->Any);

	// hash
	register_global_func(vm, "in",       &native_in_any_hash,       2, "x",   vm->Any, "h", vm->Hash);
	register_global_func(vm, "hash",     &native_hash_any,          1, "x",   vm->Any);
	register_global_func(vm, "keys",     &native_keys_hash,         1, "h",   vm->Hash);
	register_global_func(vm, "values",   &native_values_hash,       1, "h",   vm->Hash);
	register_global_func(vm, "len",      &native_len,               1, "h",   vm->Hash);
	register_global_func(vm, "[]",       &native_index_get_hash_any,        2, "h",   vm->Hash,"k", vm->Any);
	register_global_func(vm, "[]=",      &native_index_set_hash_any_any,    3, "h",   vm->Hash,"k", vm->Any, "v", vm->Any);
	register_global_func(vm, "del",      &native_index_del_hash_any,        2, "h",   vm->Hash,"k", vm->Any);

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
		v = make_string(argv[i]);
		ARRAY_ITEMS(argv_array)[i] = v;
	}
	set_global(vm, "ARGV", argv_array);
}

void ctx_init(CTX *ctx) {
	ctx->stack_ptr = 0;
	ctx->frame_ptr = 0;
	// XXX: correct sizeof?
	memset(ctx->stack, 0, sizeof(ctx->stack));
	memset(ctx->frames, 0, sizeof(ctx->frames));
}

void vm_load_bytecode(VM *vm, char *bc, size_t len) {
	// TODO: make it append, not replace. Meanwhile, make sure vm_load_bytecode() works only once.
	DEBUG_VM_API("vm_load_bytecode() VM=%p bytecode=%p\n", vm, bc);
	assert(len);
	assert(!vm->bytecode);
	assert(bc[len-1] == OP_HALT);
	vm->bytecode = bc;
}

METHOD_RESULT _vm_call(VM *vm, CTX *ctx, VALUE callable, LOCAL_VAR_INDEX argc, const VALUE *argv) {
	VALUE *local_var_ptr;
	LOCAL_VAR_INDEX lvi;
	int i;
	METHOD_RESULT mr;
	VALUE *callable_items;

	if(IS_ARRAY(callable)) {
		for(i=OBJ_LEN(callable)-1, callable_items = OBJ_DATA_PTR(callable); i>=0; i--) {
			mr = _vm_call(vm, ctx, callable_items[i], argc, argv);
			if(mr == METHOD_OK) {
				return METHOD_OK;
			}
			// Don't know how to handle other conditions yet
			assert(mr == METHOD_ARGS_MISMATCH);
		}
		return METHOD_IMPL_MISSING;
	}

	if(IS_NATIVE_METHOD(callable)) {
		// None of native method uses optional parameters for now
		if(NATIVE_METHOD_OBJ_N_OPT_PAR(callable)) {
			assert(0=="Optional parameters are not implemented yet");
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
		mr = ((VM_FUNC)OBJ_DATA_PTR(callable))(argv, &ctx->stack[ctx->stack_ptr-argc-1]);
		// Will actually be needed if/when builtins start using guards
		if(mr != METHOD_ARGS_MISMATCH) {
			ctx->stack_ptr -= argc;
		}
		return mr;
	}

	if(IS_CLOSURE(callable)) {
		// Check parameters type matching
		if(CLOSURE_OBJ_N_OPT_PAR(callable)) {
			assert(0=="Optional parameters are not implemented yet");
		}
		if(argc < CLOSURE_OBJ_N_REQ_PAR(callable)) {
			return METHOD_ARGS_MISMATCH;
		}
		if(argc > CLOSURE_OBJ_N_REQ_PAR(callable) + CLOSURE_OBJ_N_OPT_PAR(callable)) {
			return METHOD_ARGS_MISMATCH;
		}
		for(lvi=0; lvi<CLOSURE_OBJ_N_REQ_PAR(callable); lvi++) {
			// TODO: make sure second argument is type durng closure creation
			if(!obj_is_of_type(argv[lvi], CLOSURE_OBJ_PARAMS(callable)[lvi*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}

		// Setup call frame
		assert(ctx->frame_ptr < MAX_FRAMES);
		lvi = CLOSURE_OBJ_N_LOCALS(callable);
		// printf("N LOCALS %d\n", lvi);
		if(lvi) {
			ctx->frames[ctx->frame_ptr].locals = NGS_MALLOC(lvi * sizeof(VALUE));
			assert(ctx->frames[ctx->frame_ptr].locals);
			assert(argc <= lvi);
			memcpy(ctx->frames[ctx->frame_ptr].locals, &ctx->stack[ctx->stack_ptr-argc], sizeof(VALUE) * argc);
			lvi -= argc;
			for(local_var_ptr=&ctx->frames[ctx->frame_ptr].locals[argc];lvi;lvi--,local_var_ptr++) {
				SET_UNDEF(*local_var_ptr);
			}
		} else {
			ctx->frames[ctx->frame_ptr].locals = NULL;
		}
		ctx->frames[ctx->frame_ptr].closure = callable;
		// printf("INCREASING FRAME PTR\n");
		ctx->frame_ptr++;
		mr = vm_run(vm, ctx, CLOSURE_OBJ_IP(callable), &ctx->stack[ctx->stack_ptr-argc-1]);
		ctx->frame_ptr--;
		// TODO: dedup
		if(mr != METHOD_ARGS_MISMATCH) {
			ctx->stack_ptr -= argc;
		}
		return mr;
	}

	if(IS_NGS_TYPE(callable)) {
		return _vm_call(vm, ctx, NGS_TYPE_CONSTRUCTORS(callable), argc, argv);
	}

	// TODO: allow handling of calling of undefined methods by the NGS language
	dump_titled("_vm_call(): Don't know how to call", callable);
	abort();
	// TODO: return the exception
	return METHOD_EXCEPTION_OCCURED;

}

METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result) {
	VALUE v, callable;
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

main_loop:
	opcode = vm->bytecode[ip++];
#ifdef DO_NGS_DEBUG
	if(opcode <= sizeof(opcodes_names) / sizeof(char *)) {
		DEBUG_VM_RUN("main_loop IP=%i OP=%s\n", ip-1, opcodes_names[opcode]);
	}
#endif

	// Guidelines
	// * increase ip as soon as arguments extraction is finished
	switch(opcode) {
		case OP_HALT:
							goto end_main_loop;
		case OP_PUSH_NULL:
							PUSH_NULL;
							goto main_loop;
		case OP_PUSH_FALSE:
							SET_FALSE(v);
							PUSH(v);
							goto main_loop;
		case OP_PUSH_TRUE:
							SET_TRUE(v);
							PUSH(v);
							goto main_loop;
		case OP_PUSH_UNDEF:
							assert(0 == "Should not be used");
							SET_UNDEF(v);
							PUSH(v);
							goto main_loop;
		case OP_PUSH_INT:
							// Arg: n
							// In ...
							// Out: ... n
							// TODO: make it push_intSIZE maybe?
							i = *(int *) &vm->bytecode[ip];
							ip += sizeof(i);
							SET_INT(v, i);
							PUSH(v);
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
		case OP_RESOLVE_GLOBAL:
							// Probably not worh optimizing
							POP(v);
							assert(OBJ_TYPE(v) == T_STR);
							SET_INT(v, get_global_index(vm, OBJ_DATA_PTR(v), OBJ_LEN(v)));
							PUSH(v);
							goto main_loop;
		case OP_PATCH:
							// Arg: offset
							// In ... n
							// Out: ...
							// Effect: bytecode[offset] <- n
							POP(v);
							ARG(po, PATCH_OFFSET);
#ifdef DO_NGS_DEBUG
							DEBUG_VM_RUN("OP_PATCH dst_idx=%d v=%d\n", ip+po, *(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po]);
							assert(*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] == 0); // try to catch patching at invalid offset
#endif
							*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] = GET_INT(v);
							goto main_loop;
		case OP_INIT_DONE:
							// Do nothing at top level
							// TODO: handle included/required bytecode here
							goto main_loop;
		case OP_FETCH_GLOBAL:
							ARG_GVI;
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_FETCH_GLOBAL gvi=%d len=%d\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
#endif
							// TODO: report error here instead of crashing
							if(IS_UNDEF(GLOBALS[gvi])) {
								fprintf(stderr, "Global '%s' (index %d) not found\n", vm->globals_names[gvi], gvi);
								assert(0=="Global not found");
							}
							// dump_titled("FETCH_GLOBAL", GLOBALS[gvi]);
							PUSH(GLOBALS[gvi]);
							goto main_loop;
		case OP_STORE_GLOBAL:
							ARG_GVI;
							POP(v);
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_STORE_GLOBAL gvi=%d len=%zu\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
#endif
							GLOBALS[gvi] = v;
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
							POP(v);
							LOCALS[lvi] = v;
							goto main_loop;
		case OP_CALL:
							// TODO: print arguments of failed call, not just the callable
							// In (current): ... result_placeholder (null), arg1, ..., argN, argc, callable
							// In (WIP): ...
							//     result_placeholder (null),
							//     pos_arg1, ..., pos_argN,
							//     opt_arg1, ..., opt_argN,
							//     n_params_required,
							//     n_params_optional,
							//     callable
							// Out: ... result
							POP(callable);
							// dump_titled("CALLABLE", callable);
							POP(v); // number of arguments
							// POP(n_params_required); // number of arguments
							// POP(n_params_optional);
							mr = _vm_call(vm, ctx, callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							if(mr != METHOD_OK) {
								dump_titled("Failed callable", callable);
								assert(0=="Handling failed method calls is not implemented yet");
							}
							goto main_loop;
		case OP_RET:
							// TODO: check stack length
							assert(saved_stack_ptr <= ctx->stack_ptr);
							if(saved_stack_ptr < ctx->stack_ptr) {
								POP(*result);
								// dump_titled("RESULT", *result);
							} else {
								assert(0=="Function does not have result value");

							}
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
							PUSH(v);
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
							v = make_closure_obj(
									ip+jo,
									n_locals, n_params_required, n_params_optional, n_uplevels,
									&ctx->stack[ctx->stack_ptr - n_params_required*2 - n_params_optional*3]
							);
							ctx->stack_ptr -= n_params_required*2 - n_params_optional*3;
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
							assert(ctx->stack_ptr >= 2);
							v = TOP;
							if(IS_STRING(v)) {
								goto main_loop;
							}
							PUSH(v);
							mr = _vm_call(vm, ctx, (VALUE){.ptr = vm->Str}, 1, &ctx->stack[ctx->stack_ptr-1]);
							assert(mr == METHOD_OK);
							goto main_loop;
		case OP_MAKE_STR:
							// TODO: (optimization) update top of the stack instead of POP and PUSH
							POP(v);
							string_components_count = GET_INT(v);
							v = join_strings(string_components_count, &(ctx->stack[ctx->stack_ptr-string_components_count]));
							PUSH(v);
							goto main_loop;
		case OP_PUSH_EMPTY_STR:
							v = make_var_len_obj(T_STR, 1, 0);
							PUSH(v);
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
								array_push(GLOBALS[gvi], TOP);
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
								array_push(LOCALS[lvi], TOP);
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
								array_push(UPLEVELS[uvi][lvi], TOP);
							}
							goto main_loop;
		case OP_MAKE_HASH:
							POP(v);
							vlo_len = GET_INT(v);
							v = make_hash(vlo_len);
							ctx->stack_ptr -= vlo_len * 2;
							for(j=0; j<vlo_len;j++) {
								set_hash_key(v, ctx->stack[ctx->stack_ptr+j*2], ctx->stack[ctx->stack_ptr+j*2+1]);
							}
							PUSH(v);
							goto main_loop;
		case OP_TO_BOOL:
							assert(ctx->stack_ptr);
							v = TOP;
							if(IS_BOOL(v)) {
								goto main_loop;
							}
							PUSH(v);
							mr = _vm_call(vm, ctx, (VALUE){.ptr = vm->Bool}, 1, &ctx->stack[ctx->stack_ptr-1]);
							assert(mr == METHOD_OK);
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
	}
end_main_loop:
	return METHOD_OK;
}
#undef LOCALS
