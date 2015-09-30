#ifndef AST_H
#define AST_H

typedef struct ast_node {
	int type;
	char *name;
	int number;
	// Chidren
	struct ast_node *first_child;
	struct ast_node *last_child;
	struct ast_node *next_sibling;
	// Location. TODO: start/end line/col
	char *fname;
} ast_node;
#endif
