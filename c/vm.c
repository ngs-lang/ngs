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
#define RETURN_NULL {PUSH_NULL; return METHOD_OK;}
#define LOCALS (ctx->frames[ctx->frame_ptr-1].locals)

#define METHOD_MUST_HAVE_N_ARGS(n) if(n != n_args) { return METHOD_ARGS_MISMATCH; }
#define METHOD_MUST_HAVE_AT_LEAST_ARGS(n) if(n_args < n) { return METHOD_ARGS_MISMATCH; }
#define METHOD_MUST_HAVE_AT_MOST_ARGS(n) if(n_args > n) { return METHOD_ARGS_MISMATCH; }
#define METHOD_ARG_N_MUST_BE(n, what) if(!IS_ ## what(args[n])) { return METHOD_ARGS_MISMATCH; }
#define MAXIMIZE_INTO(dst, src) if((src)>(dst)) { dst = src; }

#define METHOD_BINOP_SETUP(type) \
	METHOD_MUST_HAVE_N_ARGS(2); \
	METHOD_ARG_N_MUST_BE(0, type); \
	METHOD_ARG_N_MUST_BE(1, type);

#define INT_METHOD(name, op) \
METHOD_RESULT native_ ## name(CTX *ctx, int n_args, VALUE *args) { \
	METHOD_BINOP_SETUP(INT); \
	PUSH(MAKE_INT(GET_INT(args[0]) op GET_INT(args[1]))); \
	return METHOD_OK; \
}

#define INT_CMP_METHOD(name, op) \
METHOD_RESULT native_ ## name(CTX *ctx, int n_args, VALUE *args) { \
	METHOD_BINOP_SETUP(INT); \
	PUSH(MAKE_BOOL(GET_INT(args[0]) op GET_INT(args[1]))); \
	return METHOD_OK; \
}

INT_METHOD(plus, +);
INT_METHOD(minus, -);
INT_CMP_METHOD(less, <);

METHOD_RESULT native_dump(CTX *ctx, int n_args, VALUE *args) {
	if(n_args == 1) {
		dump(args[0]);
		RETURN_NULL;
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
	ctx->ip = 0;
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

void _vm_call(CTX *ctx) {
	VALUE callable, n_args, *args;
	int i;

	POP(callable);
	IF_DEBUG(VM_RUN, dump_titled("_vm_call/callable", callable); );
	POP(n_args);
	args = NGS_MALLOC(sizeof(VALUE) * GET_INT(n_args));
	for(i=GET_INT(n_args)-1; i>=0; i--) {
		 POP(args[i]);
	}

	IF_DEBUG(VM_RUN, dump_titled("_vm_call/n_args", n_args); );

	if(IS_NATIVE_METHOD(callable)) {
		((VM_FUNC)OBJ_DATA_PTR(callable))(ctx, GET_INT(n_args), args);
		goto done;
	}

	if(IS_CLOSURE(callable)) {
		assert(ctx->frame_ptr < MAX_FRAMES);
		ctx->frames[ctx->frame_ptr].prev_ip = ctx->ip;
		if(CLOSURE_OBJ_N_LOCALS(callable)) {
			ctx->frames[ctx->frame_ptr].locals = NGS_MALLOC(CLOSURE_OBJ_N_LOCALS(callable) * sizeof(VALUE));
			assert(ctx->frames[ctx->frame_ptr].locals);
		} else {
			ctx->frames[ctx->frame_ptr].locals = NULL;
		}
		ctx->frame_ptr++;
		ctx->ip = CLOSURE_OBJ_IP(callable);
		goto done;
	}

	// TODO: allow handling of calling of undefined methods by the NGS language
	dump_titled("_vm_call(): Don't know how to call", callable);
	abort();

done: ;
}

void vm_run(VM *vm, CTX *ctx) {
	VALUE v;
	VAR_LEN_OBJECT *vlo;
	IP ip = ctx->ip;
	int i;
	unsigned char opcode;
	GLOBAL_VAR_INDEX gvi;
	PATCH_OFFSET po;
	JUMP_OFFSET jo;
	LOCAL_VAR_INDEX n_locals;
	LOCAL_VAR_INDEX lvi;
	size_t vlo_len;
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
#ifdef DO_NGS_DEBUG
							assert(OBJ_TYPE(v) == OBJ_TYPE_STRING);
#endif
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
							/* maybe pass pointer to ip for better performance? */
							ctx->ip = ip;
							_vm_call(ctx);
							ip = ctx->ip;
							goto main_loop;
		case OP_RET:
							assert(ctx->frame_ptr);
							ctx->frame_ptr--;
							ip = ctx->frames[ctx->frame_ptr].prev_ip;
							goto main_loop;
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
	return;
}
#undef LOCALS
