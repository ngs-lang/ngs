#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uthash.h>
#include "ngs.h"
#include "vm.h"
#include "obj.h"

void push(STACK **st, VALUE v);
VALUE pop(STACK **st);

#define PUSH(v) push(&ctx->stack, v)
#define POP() pop(&ctx->stack)

#define METHOD_MUST_HAVE_N_ARGS(n) if(n != n_args) { return METHOD_ARGS_MISMATCH; }
#define METHOD_ARG_N_MUST_BE(n, what) if(!IS_ ## what(args[n])) { return METHOD_ARGS_MISMATCH; }
#define MAXIMIZE_INTO(dst, src) if((src)>(dst)) { dst = src; }

#define METHOD_BINOP_SETUP(type) \
	VALUE v; \
	METHOD_MUST_HAVE_N_ARGS(2); \
	METHOD_ARG_N_MUST_BE(0, type); \
	METHOD_ARG_N_MUST_BE(1, type);

#define INT_METHOD(name, op) \
METHOD_RESULT native_ ## name(CTX *ctx, int n_args, VALUE *args) { \
	METHOD_BINOP_SETUP(INT); \
	SET_INT(v, GET_INT(args[0]) op GET_INT(args[1])); \
	PUSH(v); \
	return METHOD_OK; \
}

INT_METHOD(plus, +);
INT_METHOD(minus, -);

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
	DEBUG_VM_RUN("entering get_global_index() vm=%p name=%.*s\n", vm, name_len, name);
	index = check_global_index(vm, name, name_len, &found);
	if(found) {
		DEBUG_VM_RUN("leaving get_global_index() status=found vm=%p name=%.*s -> index=%d\n", vm, name_len, name, index);
		return index;
	}
	assert(vm->globals_len < (MAX_GLOBALS-1));
	var = NGS_MALLOC(sizeof(*var));
	var->name = NGS_MALLOC(name_len);
	memcpy(var->name, name, name_len);
	var->index = vm->globals_len++;
	HASH_ADD_KEYPTR(hh, vm->globals_indexes, var->name, name_len, var);
	vm->globals[var->index].num = V_UNDEF;
	DEBUG_VM_RUN("leaving get_global_index() status=new vm=%p name=%.*s -> index=%d\n", vm, name_len, name, var->index);
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
}

void ctx_init(CTX *ctx) {
	ctx->ip = 0;
}

void vm_load_bytecode(VM *vm, char *bc, size_t len) {
	// TODO: make it append, not replace. Meanwhile, make sure vm_load_bytecode() works only once.
	DEBUG_VM_API("vm_load_bytecode() VM=%p bytecode=%p\n", vm, bc);
	assert(len);
	assert(!vm->bytecode);
	assert(bc[len-1] == OP_HALT);
	vm->bytecode = bc;
}

// TODO: some normaml stack implementation, not linked list...
//       Actually check the performance, maybe Boehm plays nice
//       with such usage so we are losing only locality.
void inline push(STACK **st, VALUE v) {
	STACK *elt;
	elt = NGS_MALLOC(sizeof(STACK));
	memcpy(&(elt->v), &v, sizeof(v));
	elt->next = *st;
	*st = elt;
}

// Do not return pointer to stack element value because then
// the stack element can't be freed... I think.
VALUE inline pop(STACK **st) {
	VALUE v;
	STACK *stack_top;
	stack_top = *st;
	memcpy(&v, &stack_top->v, sizeof(VALUE));
	*st = stack_top->next;
	// NGS_FREE(stack_top);
	return v;
}

void _vm_call(VM *vm, CTX *ctx, IP *ip) {
	VALUE func, n_args, *args;
	int i;

	METHOD_RESULT result;

	func = POP();
	IF_DEBUG(VM_RUN,
		dump_titled("_vm_call/func", func);
	);
	n_args = POP();
	args = NGS_MALLOC(sizeof(VALUE) * GET_INT(n_args));
	for(i=GET_INT(n_args)-1; i>=0; i--) {
		args[i] = POP();
	}

	IF_DEBUG(VM_RUN,
		dump_titled("_vm_call/n_args", n_args);
	);

	if(IS_NATIVE_METHOD(func)) {
		result = ((VM_FUNC)OBJ_DATA_PTR(func))(ctx, GET_INT(n_args), args);
		goto done;
	}

	// TODO: allow handling of calling of undefined methods by the NGS language
	dump_titled("_vm_call(): Don't know how to call", func);
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
main_loop:
	opcode = vm->bytecode[ip++];
	if(opcode>=0 && opcode <= sizeof(opcodes_names) / sizeof(char *)) {
		DEBUG_VM_RUN("main_loop IP=%i OP=%s\n", ip-1, opcodes_names[opcode]);
	}
	// Guidelines
	// * increase ip as soon as arguments extraction is finished
	switch(opcode) {
		case OP_HALT:
							v = POP();
							dump_titled("halt/pop", v);
							goto end_main_loop;
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
							v = POP();
							PUSH(v);
							PUSH(v);
							goto main_loop;
		case OP_POP:
							POP();
							goto main_loop;
		case OP_RESOLVE_GLOBAL:
							v = POP();
							assert(OBJ_TYPE(v) == OBJ_TYPE_STRING);
							SET_INT(v, get_global_index(vm, OBJ_DATA_PTR(v), OBJ_LEN(v)));
							PUSH(v);
							goto main_loop;
		case OP_PATCH:
							// Arg: offset
							// In ... n
							// Out: ...
							// Effect: bytecode[offset] <- n
							v = POP();
							po = *(PATCH_OFFSET *) &vm->bytecode[ip];
							ip += sizeof(po);
							assert(*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] == 0); // try to catch patching at invalid offset
							*(GLOBAL_VAR_INDEX *)&vm->bytecode[ip+po] = GET_INT(v);
							goto main_loop;
		case OP_INIT_DONE:
							// Do nothing at top level
							// TODO: handle included/required bytecode here
							goto main_loop;
		case OP_FETCH_GLOBAL:
							gvi = *(GLOBAL_VAR_INDEX *) &vm->bytecode[ip];
							ip += sizeof(gvi);
							// DEBUG_VM_RUN("OP_FETCH_GLOBAL gvi=%d len=%d\n", gvi, vm->globals_len);
							assert(gvi < vm->globals_len);
							// TODO: report error here instead of crashing
							assert(IS_NOT_UNDEF(vm->globals[gvi]));
							PUSH(vm->globals[gvi]);
							goto main_loop;
		case OP_CALL:
							_vm_call(vm, ctx, &ip);
							goto main_loop;
		default:
							// TODO: exception
							printf("ERROR: Unknown opcode %d\n", opcode);
	}
end_main_loop:
	return;
}
