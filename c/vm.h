#ifndef VM_H
#define VM_H
#include <uthash.h>
#include "obj.h"

#define MAX_GLOBALS   (1024)
#define MAX_LOCALS    (1024)
typedef uint16_t GLOBAL_VAR_INDEX;
typedef uint16_t LOCAL_VAR_INDEX;
typedef uint16_t PATCH_OFFSET;

typedef int IP;

typedef struct stack {
	VALUE v;
	struct stack *next;
} STACK;

// TODO: maybe convert vm->globals to regular NGS hash so it could be accessible via `globals()` function.
typedef struct var_index {
	char *name;
	size_t index;
	UT_hash_handle hh;
} VAR_INDEX;

// Plan: have exactly one context per thread.
typedef struct context {
	IP ip;
	STACK *stack;
} CTX;

typedef struct vm_struct {
	char *bytecode;
	VALUE *globals;
	size_t globals_len;
	VAR_INDEX *globals_indexes;
} VM;

enum opcodes {
	OP_HALT,
	OP_PUSH_NULL,
	OP_PUSH_FALSE,
	OP_PUSH_TRUE,
	OP_PUSH_UNDEF,
	OP_PUSH_INT,
	OP_PUSH_L_STR,
	OP_DUP,
	OP_POP,
	OP_RESOLVE_GLOBAL,
	OP_PATCH,
	OP_INIT_DONE,
	OP_FETCH_GLOBAL,
	OP_STORE_GLOBAL,
	OP_CALL
};

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
	/* 14 */ "CALL"
};

typedef enum method_result_enum {
	METHOD_OK,
	METHOD_ARGS_MISMATCH
} METHOD_RESULT;

typedef METHOD_RESULT (*VM_FUNC)(CTX *ctx, int n_args, VALUE *args);
void vm_init(VM *vm);
size_t check_global_index(VM *vm, char *name, size_t name_len, int *found);
size_t get_global_index(VM *vm, char *name, size_t name_len);

// typedef int VM_INT;
#endif
