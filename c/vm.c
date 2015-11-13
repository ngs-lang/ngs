#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uthash.h>
#include "ngs.h"
#include "vm.h"
#include "obj.h"
#include "obj.c"
#include "decompile.c"

#define PUSH(v) assert(ctx->stack_ptr<MAX_STACK); ctx->stack[ctx->stack_ptr++] = v
#define POP(dst) assert(ctx->stack_ptr); ctx->stack_ptr--; dst = ctx->stack[ctx->stack_ptr]
#define PUSH_NULL PUSH((VALUE){.num=V_NULL})
#define LOCALS (ctx->frames[ctx->frame_ptr-1].locals)

#define METHOD_MUST_HAVE_N_ARGS(n) if(n != n_args) { return METHOD_ARGS_MISMATCH; }
#define METHOD_MUST_HAVE_AT_LEAST_ARGS(n) if(n_args < n) { return METHOD_ARGS_MISMATCH; }
#define METHOD_MUST_HAVE_AT_MOST_ARGS(n) if(n_args > n) { return METHOD_ARGS_MISMATCH; }
#define METHOD_ARG_N_MUST_BE(n, what) if(!IS_ ## what(args[n])) { return METHOD_ARGS_MISMATCH; }

#define METHOD_BINOP_SETUP(type) \
	METHOD_MUST_HAVE_N_ARGS(2); \
	METHOD_ARG_N_MUST_BE(0, type); \
	METHOD_ARG_N_MUST_BE(1, type);

#define INT_METHOD(name, op) \
METHOD_RESULT native_ ## name(NGS_UNUSED CTX *ctx, int n_args, VALUE *args, VALUE *result) { \
	METHOD_BINOP_SETUP(INT); \
	SET_INT(*result, GET_INT(args[0]) op GET_INT(args[1])); \
	return METHOD_OK; \
}

#define INT_CMP_METHOD(name, op) \
METHOD_RESULT native_ ## name(NGS_UNUSED CTX *ctx, int n_args, VALUE *args, VALUE *result) { \
	METHOD_BINOP_SETUP(INT); \
	SET_BOOL(*result, GET_INT(args[0]) op GET_INT(args[1])); \
	return METHOD_OK; \
}

INT_METHOD(plus, +);
INT_METHOD(minus, -);
INT_CMP_METHOD(less, <);

METHOD_RESULT native_dump(NGS_UNUSED CTX *ctx, int n_args, VALUE *args, NGS_UNUSED VALUE *result) {
	if(n_args == 1) {
		dump(args[0]);
		return METHOD_OK; // null
	}
	return METHOD_ARGS_MISMATCH;
}

size_t check_global_index(VM *vm, char *name, size_t name_len, int *found) {
	VAR_INDEX *var;
	HASH_FIND(hh, vm->globals_indexes, name, name_len, var);
	if(var) {
		*found = 1;
		return var->index;
	}
	*found = 0;
	return 0;
}

size_t get_global_index(VM *vm, char *name, size_t name_len) {
	VAR_INDEX *var;
	size_t index;
	int found;
	DEBUG_VM_RUN("entering get_global_index() vm=%p name=%.*s\n", vm, (int)name_len, name);
	index = check_global_index(vm, name, name_len, &found);
	if(found) {
		DEBUG_VM_RUN("leaving get_global_index() status=found vm=%p name=%.*s -> index=%zu\n", vm, (int)name_len, name, index);
		return index;
	}
	assert(vm->globals_len < (MAX_GLOBALS-1));
	var = NGS_MALLOC(sizeof(*var));
	var->name = NGS_MALLOC(name_len);
	memcpy(var->name, name, name_len);
	var->index = vm->globals_len++;
	HASH_ADD_KEYPTR(hh, vm->globals_indexes, var->name, name_len, var);
	vm->globals[var->index].num = V_UNDEF;
	DEBUG_VM_RUN("leaving get_global_index() status=new vm=%p name=%.*s -> index=%zu\n", vm, (int)name_len, name, var->index);
	return var->index;
}

// NOTE: couldn't make it macro - collision with uthash probably.
void register_global_func(VM *vm, char *func_name, void *func_ptr) {
	size_t index;
	OBJECT *o;
	o = NGS_MALLOC(sizeof(*o));
	o->type.num = OBJ_TYPE_NATIVE_METHOD;
	o->val.ptr = func_ptr;
	index = get_global_index(vm, func_name, strlen(func_name));
	SET_OBJ(vm->globals[index], o);
}

void vm_init(VM *vm) {
	vm->bytecode = NULL;
	vm->globals_indexes = NULL;
	vm->globals_len = 0;
	vm->globals = NGS_MALLOC(sizeof(*(vm->globals)) * MAX_GLOBALS);
	// Keep global functions registration in order.
	// This way the compiler can use globals_indexes as the beginning of
	// it's symbol table for globals.
	register_global_func(vm, "+", &native_plus);
	register_global_func(vm, "-", &native_minus);
	register_global_func(vm, "<", &native_less);
	register_global_func(vm, "dump", &native_dump);
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

METHOD_RESULT _vm_call(VM *vm, CTX *ctx, VALUE callable, LOCAL_VAR_INDEX n_args, const VALUE *args) {
	VALUE *local_var_ptr;
	LOCAL_VAR_INDEX lvi;
	int i;
	METHOD_RESULT mr;

	IF_DEBUG(VM_RUN, dump_titled("_vm_call/callable", callable); );
	DEBUG_VM_RUN("_vm_call/n_args = %d\n", n_args);

	if(IS_ARRAY(callable)) {
		// args here is used for callable array items
		for(i=OBJ_LEN(callable), args = OBJ_DATA_PTR(callable); i>=0; i--) {
			IF_DEBUG(VM_RUN, dump_titled("_vm_call/will_call", args[i]); );
			mr = _vm_call(vm, ctx, args[i], n_args, args);
			if(mr == METHOD_OK) {
				return METHOD_OK;
			}
			// Don't know how to handle other condittions yet
			assert(mr == METHOD_ARGS_MISMATCH);
		}
		return METHOD_IMPL_MISSING;
	}

	if(IS_NATIVE_METHOD(callable)) {
		// TODO: check whether everything works find with n_args == 0
		mr = ((VM_FUNC)OBJ_DATA_PTR(callable))(ctx, n_args, args, &ctx->stack[ctx->stack_ptr-n_args-1]);
		if(mr != METHOD_ARGS_MISMATCH) {
			ctx->stack_ptr -= n_args;
		}
		return mr;
	}

	if(IS_CLOSURE(callable)) {
		assert(ctx->frame_ptr < MAX_FRAMES);
		lvi = CLOSURE_OBJ_N_LOCALS(callable);
		if(lvi) {
			ctx->frames[ctx->frame_ptr].locals = NGS_MALLOC(lvi * sizeof(VALUE));
			assert(ctx->frames[ctx->frame_ptr].locals);
			// TODO: make it number of arguments, not local variables
			// TODO: throw ngs exception on arguments mismatch
			assert(n_args <= lvi);
			memcpy(ctx->frames[ctx->frame_ptr].locals, &ctx->stack[ctx->stack_ptr-n_args], sizeof(VALUE) * n_args);
			lvi -= n_args;
			for(local_var_ptr=&ctx->frames[ctx->frame_ptr].locals[n_args];lvi;lvi--,local_var_ptr++) {
				SET_UNDEF(*local_var_ptr);
			}
		} else {
			// TODO: throw ngs exception on arguments mismatch
			assert(n_args == 0);
			ctx->frames[ctx->frame_ptr].locals = NULL;
		}
		ctx->frame_ptr++;
		mr = vm_run(vm, ctx, CLOSURE_OBJ_IP(callable), &ctx->stack[ctx->stack_ptr-n_args-1]);
		// TODO: dedup
		if(mr != METHOD_ARGS_MISMATCH) {
			ctx->stack_ptr -= n_args;
		}
		return mr;
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
	LOCAL_VAR_INDEX n_locals;
	LOCAL_VAR_INDEX lvi;
	size_t vlo_len;
	METHOD_RESULT mr;
	size_t saved_stack_ptr = ctx->stack_ptr;
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
							// printf("LSTR @ %p\n", &vm->bytecode[ip]);
							vlo = NGS_MALLOC(sizeof(*vlo));
							vlo->len = (size_t) vm->bytecode[ip];
							vlo->base.type.num = OBJ_TYPE_STRING;
							vlo->base.val.ptr = NGS_MALLOC(vlo->len);
							memcpy(vlo->base.val.ptr, &(vm->bytecode[ip+1]), vlo->len);
							ip += 1 + vm->bytecode[ip];
							SET_OBJ(v, vlo);
							PUSH(v);
							goto main_loop;
		case OP_DUP:
							// TODO: optimize later
							POP(v);
							PUSH(v);
							PUSH(v);
							goto main_loop;
		case OP_POP:
							POP(v);
							goto main_loop;
		case OP_RESOLVE_GLOBAL:
							POP(v);
							assert(OBJ_TYPE(v) == OBJ_TYPE_STRING);
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
							// TODO: report error here instead of crashing
							assert(IS_NOT_UNDEF(vm->globals[gvi]));
							// dump_titled("FETCH_GLOBAL", vm->globals[gvi]);
#endif
							PUSH(vm->globals[gvi]);
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
							vm->globals[gvi] = v;
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
							// In: ... result_placeholder (null), arg1, ..., argN, num_of_args, callable
							// Out: ... result
							POP(callable);
							POP(v); // number of arguments
							mr = _vm_call(vm, ctx, callable, GET_INT(v), &ctx->stack[ctx->stack_ptr-GET_INT(v)]);
							assert(mr == METHOD_OK);
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
							v = make_var_len_obj(sizeof(VALUE), vlo_len);
							if(vlo_len) {
								memcpy(OBJ_DATA_PTR(v), &(ctx->stack[ctx->stack_ptr-vlo_len]), sizeof(VALUE)*vlo_len);
							}
							PUSH(v);
							goto main_loop;
		case OP_MAKE_CLOSURE:
							jo = *(JUMP_OFFSET *) &vm->bytecode[ip];
							ip += sizeof(jo);
							n_locals = *(LOCAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(n_locals);
							PUSH(make_closure_obj(ip+jo, n_locals));
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
	}
end_main_loop:
	return METHOD_OK;
}
#undef LOCALS
