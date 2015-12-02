#ifndef VM_H
#define VM_H
#include <utarray.h>
#include <uthash.h>
#include "ngs.h"
#include "obj.h"

#define NGS_UNUSED __attribute__((unused))

#define MAX_STACK     (1024)
#define MAX_FRAMES      (64)
typedef uint16_t PATCH_OFFSET;
typedef int16_t JUMP_OFFSET;

typedef int IP;

typedef struct var_index {
	char *name;
	GLOBAL_VAR_INDEX index;
	UT_hash_handle hh;
} VAR_INDEX;

// WIP
typedef struct interned_string {
	char *str;
	UT_hash_handle hh;
} INTERNED_STRING;

typedef struct frame {
	IP prev_ip;
	size_t prev_stack_ptr;
	// TODO: smarter allocation of locals when can't be captured by clousre,
	//       probably on stack.
	VALUE *locals;
	// int n_local_vars; // needed
} FRAME;

// Plan: have exactly one context per thread.
typedef struct context {
	VALUE stack[MAX_STACK];
	size_t stack_ptr;

	FRAME frames[MAX_FRAMES];
	size_t frame_ptr;
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
	OP_FETCH_LOCAL,
	OP_STORE_LOCAL,
	OP_CALL,
	OP_RET,
	OP_JMP,
	OP_JMP_TRUE,
	OP_JMP_FALSE,
	OP_MAKE_ARR,
	OP_MAKE_CLOSURE,
	OP_TO_STR,
	NUMBER_OF_OPCODES,
};

extern char *opcodes_names[NUMBER_OF_OPCODES];

typedef enum method_result_enum {
	METHOD_OK,
	METHOD_ARGS_MISMATCH,
	METHOD_IMPL_MISSING,
	METHOD_EXCEPTION_OCCURED,
} METHOD_RESULT;

typedef METHOD_RESULT (*VM_FUNC)(CTX *ctx, LOCAL_VAR_INDEX argc, const VALUE *argv, VALUE *result);
void vm_init(VM *vm);
void vm_load_bytecode(VM *vm, char *bc, size_t len);
void ctx_init(CTX *ctx);
GLOBAL_VAR_INDEX check_global_index(VM *vm, const char *name, size_t name_len, int *found);
GLOBAL_VAR_INDEX get_global_index(VM *vm, const char *name, size_t name_len);

static const UT_icd ut_value_icd _UNUSED_ = {sizeof(VALUE),NULL,NULL,NULL};
// typedef int VM_INT;
METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result);
#endif
