#ifndef VM_H
#define VM_H
#include <uthash.h>
#include "obj.h"

typedef int IP;

typedef struct stack {
	VALUE v;
	struct stack *next;
} STACK;

typedef struct variable {
	char *name; // intern?
	VALUE v;
	UT_hash_handle hh;
} VAR;

// Plan: have exactly one context per thread.
typedef struct context {
	IP ip;
	STACK *stack;
} CTX;

typedef struct vm_struct {
	char *bytecode;
	VAR *globals;
} VM;

enum instructions {
	OP_HALT,
	OP_PUSH_INT,
	OP_PUSH_L_STR,
	OP_FETCH_GLOBAL,
	OP_CALL,
	OP_FILLER = 255
};

char *opcodes_names[] = {
	"HALT",
	"PUSH_INT",
	"PUSH_L_STR",
	"FETCH_GLOBAL",
	"CALL",
};

typedef enum method_result_enum {
	METHOD_OK,
	METHOD_ARGS_MISMATCH
} METHOD_RESULT;

typedef METHOD_RESULT (*VM_FUNC)(CTX *ctx, int n_args, VALUE *args);

// typedef int VM_INT;
#endif
