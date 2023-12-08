typedef struct ast_node_ll {
	ast_node *node;
	ast_node_type type;
	struct ast_node_ll *next;
} AST_NODE_TYPE_LL;

#define YYSTYPE ast_node*
#define YY_CTX_MEMBERS \
	int fail_pos, seq_pos; \
	char *fail_rule; \
	int lines, lines_postions[YY_MAX_LINES]; \
	FILE *input_file; \
	AST_NODE_TYPE_LL *nodes_for_type_change; \
	char *source_file_name;

void position_to_line_col();
// void position_to_line_col(yycontext *yy, int pos, int result[]); --> error: unknown type name 'YYSTYPE'
