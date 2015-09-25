#ifndef VM_H
#define VM_H
#include <uthash.h>
#include "obj.h"

#define MAX_GLOBALS   (1024)
#define MAX_LOCALS    (1024)
typedef uint16_t GLOBAL_VAR_INDEX;
typedef uint16_t LOCAL_VAR_INDEX;

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
	OP_PUSH_INT,
	OP_PUSH_L_STR,
	OP_DUP,
	OP_POP,
	OP_RESOLVE_GLOBAL,
	OP_PATCH,
	OP_INIT_DONE,
	OP_FETCH_GLOBAL,
	OP_CALL
};

char *opcodes_names[] = {
	"HALT",
	"PUSH_INT",
	"PUSH_L_STR",
	"DUP",
	"POP",
	"RESOLVE_GLOBAL",
	"PATCH",
	"INIT_DONE",
	"FETCH_GLOBAL",
	"CALL"
};

typedef enum method_result_enum {
	METHOD_OK,
	METHOD_ARGS_MISMATCH
} METHOD_RESULT;

typedef METHOD_RESULT (*VM_FUNC)(CTX *ctx, int n_args, VALUE *args);
void vm_init(VM *vm);

// typedef int VM_INT;
#endif
