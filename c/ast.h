typedef struct ast_node {
	int type;
	obj val;
	// Chidren
	struct ast_node *children;
	struct ast_node *last_child;
	// Location. TODO: start/end line/col
	char *fname;
} ast_node;
