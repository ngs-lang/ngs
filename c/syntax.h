typedef struct ast_node_ll {
	ast_node *node;
	ast_node_type type;
	struct ast_node_ll *next;
} AST_NODE_TYPE_LL;

#define MAX_COLLECTOR_PTR (32)

#define YYSTYPE ast_node*
#define YY_CTX_MEMBERS \
	int fail_pos, seq_pos; \
	char *fail_rule; \
	int lines, lines_postions[YY_MAX_LINES]; \
	FILE *input_file; \
	AST_NODE_TYPE_LL *nodes_for_type_change;
