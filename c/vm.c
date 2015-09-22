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

void native_plus(CTX *ctx, int n_args, VALUE *args) {
	VALUE v;
	assert(n_args == 2);
	SET_INT(v, GET_INT(args[0]) + GET_INT(args[1]));
	PUSH(v);
}

// TODO: use HASH_ADD_PTR and interned symbols
// NOTE: couldn't make it macro - collision with uthash probably.
void _register_global_func(VM *vm, char *func_name, void *func_ptr) {
	VAR *var;
	var = NGS_MALLOC(sizeof(VAR));
	var->name = strdup(func_name);
	var->v.ptr = func_ptr;
	HASH_ADD_KEYPTR(hh, vm->globals, var->name, strlen(var->name), var);
}

void vm_init(VM *vm) {
	vm->bytecode = NULL;
	_register_global_func(vm, "+", &native_plus);
}

void ctx_init(CTX *ctx) {
	ctx->ip = 0;
}

void vm_load_bytecode(VM *vm, char *bc, IP len) {
	// TODO: make it append, not replace. Meanwhile, make sure vm_load_bytecode() works only once.
	assert(!vm->bytecode);
	vm->bytecode = bc;
	printf("[debug] Loading bytecode. VM at %p BC %p\n", vm, bc);
}

// TODO: some normaml stack implementation, not linked list
void push(STACK **st, VALUE v) {
	STACK *elt;
	elt = NGS_MALLOC(sizeof(STACK));
	memcpy(&(elt->v), &v, sizeof(v));
	elt->next = *st;
	*st = elt;
}

// Do not return pointer to stack element value because then
// the stack element can't be freed... I think.
VALUE pop(STACK **st) {
	VALUE v;
	STACK *stack_top;
	stack_top = *st;
	memcpy(&v, &stack_top->v, sizeof(VALUE));
	*st = stack_top->next;
	// NGS_FREE(stack_top);
	return v;
}

void _vm_call(VM *vm, CTX *ctx, IP *ip) {
	VALUE func_name, n_args, *args, f;
	VAR *func;
	int i;

	char len, *ch, asciiz_func_name[256];

	func_name = POP();
	n_args = POP();
	args = NGS_MALLOC(sizeof(VALUE) * GET_INT(n_args));
	for(i=GET_INT(n_args)-1; i>=0; i--) {
		args[i] = POP();
	}

	GET_LSTR(ch, func_name);
	len = *ch;
	memcpy(asciiz_func_name, ch+1, len);
	asciiz_func_name[len] = 0;
	printf("ASCIIZ [%s]\n", asciiz_func_name);

	for(func = vm->globals; func; func=func->hh.next) {
		printf("FUNC [%s]\n", func->name);

	}

	HASH_FIND_STR(vm->globals, asciiz_func_name, func);
	assert(func);
	printf("NARGS %d\n", GET_INT(n_args));
	((VM_FUNC)func->v.ptr)(ctx, GET_INT(n_args), args);
}

void vm_run(VM *vm, CTX *ctx) {
	int i, opcode;
	VALUE v;
	OBJECT *o;
	IP ip = ctx->ip;
main_loop:
	// printf("[debug] main loop. ip %i\n", ip);
	opcode = vm->bytecode[ip++];
	if(opcode>=0 && opcode <= sizeof(opcodes_names) / sizeof(char *)) {
		printf("[debug] IP=%i OP=%s\n", ip, opcodes_names[opcode]);
	}
	switch(opcode) {
		case OP_HALT:
							v = POP();
							// printf("HALT V=%d\n", GET_INT(v));
							goto end_main_loop;
		case OP_PUSH_L_STR:
							// TODO: duplicate the string?
							assert((ip & 7) == 0); // 8 byte alignment as we will point to the Lstring here
							printf("LSTR @ %p\n", &vm->bytecode[ip]);
							SET_LSTR(v, &(vm->bytecode[ip]));
							PUSH(v);
							// Skip length byte and the string that is stored in the length byte
							ip += 1 + vm->bytecode[ip];
							goto main_loop;
		case OP_PUSH_INT:
							i = *(int *) &vm->bytecode[ip];
							ip += sizeof(i);
							SET_INT(v, i);
							PUSH(v);
							goto main_loop;
		case OP_CALL:
							_vm_call(vm, ctx, &ip);
							goto main_loop;
		default:
							printf("ERROR: Unknown opcode %d\n", opcode);
	}
end_main_loop:
	return;
}
