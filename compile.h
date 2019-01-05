#ifndef COMPILE_H
#define COMPILE_H

#define SYMBOL_OFFSETS_INITIAL_SIZE (16)
#define COMPILE_INITIAL_BUF_SIZE (16384)
#define COMPILE_MAX_FUNC_DEPTH      (16)
#define COMPILE_MAX_FILL_IN_LEN     (16)

#include "vm.h"

typedef struct symbol {
	int is_predefined_global;
	unsigned int index;
	VALUE offsets;
} SYMBOL;

enum identifier_type {
	NO_IDENTIFIER=0,
	GLOBAL_IDENTIFIER,
	LOCAL_IDENTIFIER,
	UPVAR_IDENTIFIER,
};

typedef struct identifier_info {
	enum identifier_type type;
	unsigned int index; // assuming sizeof(unsigned int) > max(sizeof(GLOBAL_VAR_INDEX), sizeof(LOCAL_VAR_INDEX))
	UPVAR_INDEX uplevel;
} IDENTIFIER_INFO;

typedef struct compilation_context {
	VM vm;
	VALUE globals;
	VALUE locals[COMPILE_MAX_FUNC_DEPTH];
	VALUE identifiers_scopes[COMPILE_MAX_FUNC_DEPTH];
	LOCAL_VAR_INDEX n_locals[COMPILE_MAX_FUNC_DEPTH];
	UPVAR_INDEX n_uplevels[COMPILE_MAX_FUNC_DEPTH];
	int stack_depth[COMPILE_MAX_FUNC_DEPTH];
	int locals_ptr;

	char *source_file_name;
	source_tracking_entry *source_tracking_entries;
	int source_tracking_entries_allocated;
	int source_tracking_entries_count;

	size_t fill_in_break_addrs[COMPILE_MAX_FILL_IN_LEN];
	int fill_in_break_addrs_ptr;
	size_t fill_in_continue_addrs[COMPILE_MAX_FILL_IN_LEN];
	int fill_in_continue_addrs_ptr;

	ast_node *ns[COMPILE_MAX_FUNC_DEPTH];

} COMPILATION_CONTEXT;

SYMBOL *get_symbol_table_entry(VALUE st, char *name, int create_if_not_exists, int *created);
char *compile(ast_node *node, char *source_file_name, size_t *len);
#endif
