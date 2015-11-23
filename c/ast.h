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

#endif
