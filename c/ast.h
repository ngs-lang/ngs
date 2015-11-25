#ifndef AST_H
#define AST_H

typedef struct ast_node_location {
	int first_line;
	int first_column;
	int last_line;
	int last_column;
	int is_generated;
} ast_node_location;

typedef struct ast_node {
	int type;
	char *name;
	int number;
	// Chidren
	struct ast_node *first_child;
	struct ast_node *last_child;
	struct ast_node *next_sibling;
	struct ast_node_location location;
} ast_node;

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
	PARAMS_NODE,
	PARAM_NODE,
};

extern char *NGS_AST_NODE_TYPES_NAMES[];

void print_ast(ast_node *node, int level);

#endif
