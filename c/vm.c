#include <assert.h>
#include <stdarg.h>
#include "ngs.h"
#include "vm.h"

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
};


#define PUSH(v) assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr++] = v
#define POP(dst) assert(ctx->stack_ptr); ctx->stack_ptr--; dst = ctx->stack[ctx->stack_ptr]
#define DUP assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr] = ctx->stack[ctx->stack_ptr-1]; ctx->stack_ptr++;
#define REMOVE_TOP assert(ctx->stack_ptr); ctx->stack_ptr--; 
#define PUSH_NULL PUSH((VALUE){.num=V_NULL})
#define GLOBALS (vm->globals)
#define LOCALS (ctx->frames[ctx->frame_ptr-1].locals)

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
INT_CMP_METHOD(less, <);

METHOD_RESULT native_dump METHOD_PARAMS {
	dump(argv[0]);
	SET_NULL(*result);
	return METHOD_OK; // null
}

METHOD_RESULT native_plus_arr_arr METHOD_PARAMS {
	*result = make_array(ARG_LEN(0) + ARG_LEN(1));
	memcpy(ARRAY_ITEMS(*result)+0, ARG_DATA_PTR(0), sizeof(VALUE)*ARG_LEN(0));
	memcpy(ARRAY_ITEMS(*result)+ARG_LEN(0), OBJ_DATA_PTR(argv[1]), sizeof(VALUE)*ARG_LEN(1));
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
		argv = NGS_MALLOC(sizeof(VALUE) * 2);
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
		// dump_titled("register_global_func", GLOBALS[index]);
		return;
	}
	assert(0 == "register_global_func fail");
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

void vm_init(VM *vm) {
	vm->bytecode = NULL;
	vm->globals_indexes = NULL;
	vm->globals_len = 0;
	vm->globals = NGS_MALLOC(sizeof(*(vm->globals)) * MAX_GLOBALS);
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
	register_global_func(vm, "+",    &native_plus_arr_arr,  2, "a",   vm->Arr, "b", vm->Arr);
	register_global_func(vm, "+",    &native_plus_int_int,  2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "-",    &native_minus_int_int, 2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "<",    &native_less_int_int,  2, "a",   vm->Int, "b", vm->Int);
	register_global_func(vm, "dump", &native_dump,          1, "obj", vm->Any);
	register_global_func(vm, "Bool", &native_Bool_any,      1, "x",   vm->Any);
	register_global_func(vm, "Str",  &native_Str_int,       1, "n",   vm->Int);
	register_global_func(vm, "is",   &native_is_any_type,   2, "obj", vm->Any, "t", vm->Type);
	register_global_func(vm, "not",  &native_not_any,       1, "x",   vm->Any);
}

void ctx_init(CTX *ctx) {
	ctx->stack_ptr = 0;
	ctx->frame_ptr = 0;
#ifdef NGS_DEBUG_FLAGS
	memset(ctx->stack, 0, sizeof(ctx->stack));
	memset(ctx->frames, 0, sizeof(ctx->frames));
#endif
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

	IF_DEBUG(VM_RUN, dump_titled("_vm_call/callable", callable); );
	DEBUG_VM_RUN("_vm_call/argc = %d\n", argc);

	if(IS_ARRAY(callable)) {
		for(i=OBJ_LEN(callable)-1, callable_items = OBJ_DATA_PTR(callable); i>=0; i--) {
			IF_DEBUG(VM_RUN, dump_titled("_vm_call/will_call", callable_items[i]); );
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
		if(argc != NATIVE_METHOD_OBJ_N_REQ_PAR(callable)) {
			return METHOD_ARGS_MISMATCH;
		}
		for(lvi=0; lvi<NATIVE_METHOD_OBJ_N_REQ_PAR(callable); lvi++) {
			// TODO: make sure second argument is type durng closure creation
			if(!obj_is_of_type(argv[lvi], NATIVE_METHOD_OBJ_PARAMS(callable)[lvi*2+1])) {
				return METHOD_ARGS_MISMATCH;
			}
		}
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
		ctx->frame_ptr++;
		mr = vm_run(vm, ctx, CLOSURE_OBJ_IP(callable), &ctx->stack[ctx->stack_ptr-argc-1]);
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
	size_t vlo_len;
	METHOD_RESULT mr;
	size_t saved_stack_ptr = ctx->stack_ptr;
	size_t string_components_count;

	// for OP_MAKE_CLOSURE
	LOCAL_VAR_INDEX n_locals, n_params_required, n_params_optional;
	VALUE *params = NULL;

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
							// POP(v);
							// dump_titled("halt/pop", v);
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
							po = *(PATCH_OFFSET *) &vm->bytecode[ip];
							ip += sizeof(po);
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
							gvi = *(GLOBAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(gvi);
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_FETCH_GLOBAL gvi=%d len=%d\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
#endif
							// TODO: report error here instead of crashing
							if(IS_UNDEF(GLOBALS[gvi])) {
								printf("Global %d not found\n", gvi);
								assert(0=="Global not found");
							}
							// dump_titled("FETCH_GLOBAL", GLOBALS[gvi]);
							PUSH(GLOBALS[gvi]);
							goto main_loop;
		case OP_STORE_GLOBAL:
							POP(v);
							gvi = *(GLOBAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(gvi);
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_STORE_GLOBAL gvi=%d len=%zu\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
#endif
							GLOBALS[gvi] = v;
							goto main_loop;
		case OP_FETCH_LOCAL:
							lvi = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							// printf("FETCH LOCAL %d !!!\n", lvi);
							ip += sizeof(lvi);
							assert(IS_NOT_UNDEF(LOCALS[lvi]));
							PUSH(LOCALS[lvi]);
							goto main_loop;
		case OP_STORE_LOCAL:
							lvi = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(lvi);
							// printf("STORE LOCAL %d %p!!!\n", lvi, LOCALS);
							POP(v);
							LOCALS[lvi] = v;
							goto main_loop;
		case OP_CALL:
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
							POP(v); // number of arguments
							// POP(n_params_required); // number of arguments
							// POP(n_params_optional);
							mr = _vm_call(vm, ctx, callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							if(mr != METHOD_OK) {
								assert(0=="Handling failed method calls is not implemented yet");
							}
							goto main_loop;
		case OP_RET:
							// TODO: check stack length
							// TODO: return value
							assert(saved_stack_ptr <= ctx->stack_ptr);
							if(saved_stack_ptr < ctx->stack_ptr) {
								POP(*result);
								// dump_titled("RESULT", *result);
							}
							return METHOD_OK;
		case OP_JMP:
do_jump:
							jo = *(JUMP_OFFSET *) &vm->bytecode[ip];
							// DEBUG_VM_RUN("JUMP OFFSET %d\n", jo);
							ip += sizeof(jo);
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
							jo = *(JUMP_OFFSET *) &vm->bytecode[ip];
							ip += sizeof(jo);
							n_locals = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(n_locals);
							POP(v);
							n_params_optional = GET_INT(v);
							POP(v);
							n_params_required = GET_INT(v);
							params = &ctx->stack[ctx->stack_ptr - n_params_required*2 - n_params_optional*3];
							PUSH(make_closure_obj(ip+jo, n_locals, n_params_required, n_params_optional, params));
							goto main_loop;
		case OP_TO_STR:
							assert(ctx->stack_ptr);
							if(IS_STRING(ctx->stack[ctx->stack_ptr-1])) {
								goto main_loop;
							}
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
							gvi = *(GLOBAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(gvi);
							PUSH(MAKE_BOOL(IS_NOT_UNDEF(GLOBALS[gvi])));
							goto main_loop;
		case OP_LOCAL_DEF_P:
							lvi = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(lvi);
							PUSH(MAKE_BOOL(IS_NOT_UNDEF(LOCALS[lvi])));
							goto main_loop;
		case OP_DEF_GLOBAL_FUNC:
							// Arg: gvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							gvi = *(GLOBAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(gvi);
#ifdef DO_NGS_DEBUG
							// DEBUG_VM_RUN("OP_STORE_GLOBAL gvi=%d len=%zu\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
#endif
							if(IS_UNDEF(GLOBALS[gvi])) {
								GLOBALS[gvi] = make_array_with_values(1, &(ctx->stack[ctx->stack_ptr-1]));
							} else {
								array_push(GLOBALS[gvi], ctx->stack[ctx->stack_ptr-1]);
							}
							goto main_loop;
		case OP_DEF_LOCAL_FUNC:
							// Arg: lvi
							// In: ..., closure
							// Out: ..., closure
							assert(ctx->stack_ptr);
							lvi = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(lvi);
							if(IS_UNDEF(LOCALS[lvi])) {
								LOCALS[lvi] = make_array_with_values(1, &(ctx->stack[ctx->stack_ptr-1]));
							} else {
								array_push(LOCALS[lvi], ctx->stack[ctx->stack_ptr-1]);
							}
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
	}
end_main_loop:
	return METHOD_OK;
}
#undef LOCALS
