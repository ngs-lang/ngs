#include "ngs.h"
#include "ast.h"
#include "obj.h"

char *NGS_AST_NODE_TYPES_NAMES[] = {
	NULL,
	"assignment",
	"identifier",
	"int",
	"real",
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
	"str_comp_expansion",
	"str_comp_splat_expansion",
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
	"upvar",
	"global",
	"hash",
	"index",
	"field",
	"ns",
	"return",
	"and",
	"or",
	"tand",
	"tor",
	"arr_splat",
	"hash_splat",
	"guard",
	"try_catch",
	"throw",
	"commands_pipeline",
	"commands_pipe",
	"command",
	"break",
	"continue",
	"redir",
	"switch",
	"super",
	"regexp",
	"table",
	"set_ns",
	"get_ns",
	"section"
};


#define AST_NODE_INFO_STR_LEN (128)

void print_ast(ast_node *node, int level) {
	ast_node *child;
	char *name = "(n/a)";
	char info[AST_NODE_INFO_STR_LEN] = "(n/a)";

	switch(node->type) {
		case CALL_NODE:
		case INT_NODE:
			snprintf(info, AST_NODE_INFO_STR_LEN-1, "%ld", node->number);
			break;
		case REAL_NODE:
			snprintf(info, AST_NODE_INFO_STR_LEN-1, NGS_REAL_FMT, (*(NGS_REAL *)node->data));
			break;
	}

	if(node->name) {
		name = node->name;
	}

	printf("[DEBUG] %*s+ AST node at %p, type %s, name %s, info %s, data %s, source_location %d:%d-%d:%d%s\n", level*2, "", \
		node, NGS_AST_NODE_TYPES_NAMES[node->type], name, info, \
		node->data ? "exists" : "does not exist", \
		node->location.first_line, node->location.first_column, \
		node->location.last_line,  node->location.last_column, \
		node->location.is_generated ? " (generated)" : "");
	for(child = node->first_child; child; child=child->next_sibling) {
		print_ast(child, level+1);
	}
}
