#ifndef AST_H
#define AST_H

typedef struct {
	uint32_t first_line;
	uint32_t first_column;
	uint32_t last_line;
	uint32_t last_column;
	int is_generated;
} ast_node_location;

typedef enum {
	ASSIGNMENT_NODE=1,
	IDENTIFIER_NODE,
	INT_NODE,
	REAL_NODE,
	EXPRESSIONS_NODE,
	FOR_NODE,
	CALL_NODE,
	EMPTY_NODE,
	ARR_LIT_NODE,
	FUNC_NODE,
	PARAMS_NODE,
	PARAM_NODE,
	STR_COMPS_NODE,
	STR_COMP_IMM_NODE,
	STR_COMP_EXPANSION_NODE,
	STR_COMP_SPLAT_EXPANSION_NODE,
	NULL_NODE,
	TRUE_NODE,
	FALSE_NODE,
	IF_NODE,
	ASSIGN_DEFAULT_NODE,
	WHILE_NODE,
	ARGS_NODE,
	ARG_NODE,
	LOCAL_NODE,
	UPVAR_NODE,
	GLOBAL_NODE,
	HASH_LIT_NODE,
	INDEX_NODE,
	FIELD_NODE,
	NS_NODE,
	RETURN_NODE,
	AND_NODE,
	OR_NODE,
	TAND_NODE,
	TOR_NODE,
	ARR_SPLAT_NODE,
	HASH_SPLAT_NODE,
	GUARD_NODE,
	TRY_CATCH_NODE,
	THROW_NODE,
	COMMANDS_PIPELINE_NODE,
	COMMANDS_PIPE_NODE,
	COMMAND_NODE,
	BREAK_NODE,
	CONTINUE_NODE,
	REDIR_NODE,
	SWITCH_NODE,
	SUPER_NODE,
	REGEXP_NODE,
	SET_NS_NODE,
	GET_NS_NODE,
	SECTION_NODE,
	NUMBER_OF_AST_NODE_TYPES,
} ast_node_type;

typedef struct ast_node {
	ast_node_type type;
	char *name;
	char *warning;
	// MAYBE TODO: make it int but store parsed long int values somewhere else
	long int number;
	void *data;
	// Children
	struct ast_node *first_child;
	struct ast_node *last_child;
	struct ast_node *next_sibling;
	ast_node_location location;
} ast_node;

typedef enum {
	SWITCH_NODE_SWITCH  = 2,
	SWITCH_NODE_ESWITCH = 3,
	SWITCH_NODE_MATCH   = 4,
	SWITCH_NODE_EMATCH  = 5,
	SWITCH_NODE_COND    = 6,
	SWITCH_NODE_ECOND   = 7,
} switch_node_subtype;

extern char *NGS_AST_NODE_TYPES_NAMES[NUMBER_OF_AST_NODE_TYPES];

void print_ast(ast_node *node, int level);

#endif
