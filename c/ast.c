#include <assert.h>
#include <stdio.h>
#include "ast.h"
#include "parser.h"
#include "obj.h"
#include "ngs.h"
#include "compile.h"

#define AST_NODE_INFO_STR_LEN (128)

void print_ast(ast_node *node, int level) {
	ast_node *child;
	char *name = "(n/a)";
	char info[AST_NODE_INFO_STR_LEN] = "(n/a)";

	switch(node->type) {
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
