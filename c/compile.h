#ifndef COMPILE_H
#define COMPILE_H

#define SYMBOL_OFFSETS_INITIAL_SIZE (16)
#define COMPILE_INITIAL_BUF_SIZE (16384)
#define COMPILE_MAX_FUNC_DEPTH (16)

#include <utarray.h>

static const UT_icd ut_size_t_icd _UNUSED_ = {sizeof(size_t),NULL,NULL,NULL};

typedef struct symbol_table {
	char *name;
	UT_hash_handle hh;
	UT_array *offsets;
	int is_predefinded_global;
	size_t index;
} SYMBOL_TABLE;

enum identifier_type {
	NO_IDENTIFIER=0,
	GLOBAL_IDENTIFIER,
	LOCAL_IDENTIFIER,
	UPVAR_IDENTIFIER,
};

typedef struct identifier_info {
	enum identifier_type type;
	unsigned int index; // assuming sizeof(unsigned int) > max(sizeof(GLOBAL_VAR_INDEX), sizeof(LOCAL_VAR_INDEX))
	unsigned int uplevel;
} IDENTIFIER_INFO;

typedef struct compilation_context {
	VM vm;
	SYMBOL_TABLE *globals;
	SYMBOL_TABLE **locals;
	LOCAL_VAR_INDEX *n_locals;
	int locals_ptr;
	int in_function; /* as opposed to global scope */
} COMPILATION_CONTEXT;

enum ast_node_type {
	ASSIGNMENT_NODE=1,
	IDENTIFIER_NODE,
	NUMBER_NODE,
	EXPRESSIONS_NODE,
	FOR_NODE,
	CALL_NODE,
	EMPTY_NODE,
	ARR_LIT_NODE,
	FUNC_NODE,
	ARGS_NODE,
};

char *NGS_AST_NODE_TYPES_NAMES[] = {
	NULL,
	"assignment",
	"identifier",
	"number",
	"expressions",
	"for",
	"call",
	"empty",
	"array",
	"func",
	"args",
};

#endif
