#include "ngs.h"
#include "ast.h"

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
	"params",
	"param",
	"str_comps",
	"str_comp_imm",
	"null",
	"true",
	"false",
	"defined",
	"if",
	"assign_default",
	"while",
	"args",
	"arg",
	"local",
	"hash",
	"index",
	"return",
	"and",
	"or",
	"arr_splat",
	"hash_splat",
	"guard",
	"try_catch",
	"throw",
	"command",
	"break",
	"continue",
};


#define AST_NODE_INFO_STR_LEN (128)

#ifdef DO_NGS_DEBUG
void print_ast(ast_node *node, int level) {
	ast_node *child;
	char *name = "(n/a)";
	char info[AST_NODE_INFO_STR_LEN] = "(n/a)";

	switch(node->type) {
		case CALL_NODE:
		case NUMBER_NODE: snprintf(info, AST_NODE_INFO_STR_LEN-1, "%d", node->number); break;
	}

	if(node->name) {
		name = node->name;
	}

	DEBUG_PARSER("%*s+ AST node at %p, type %s, name %s, info %s, source_location %d:%d-%d:%d%s\n", level*2, "", \
		node, NGS_AST_NODE_TYPES_NAMES[node->type], name, info, \
		node->location.first_line, node->location.first_column, \
		node->location.last_line,  node->location.last_column, \
		node->location.is_generated ? " (generated)" : "");
	for(child = node->first_child; child; child=child->next_sibling) {
		print_ast(child, level+1);
	}
}
#endif
