#include <stdio.h>
#include "ast.h"
#include "parser.h"
#include "obj.h"

#define AST_NODE_INFO_STR_LEN (128)

void print_ast(ast_node *node, int level) {
	ast_node *child;
	char *type_ = NULL;
	char *name = "(n/a)";
	char info[AST_NODE_INFO_STR_LEN] = "(n/a)";

	switch(node->type) {
		case NUMBER: type_="number"; snprintf(info, AST_NODE_INFO_STR_LEN-1, "%d", GET_INT(node->val)); break;
		case BINOP: type_="binop"; break;
	}

	if(node->name) {
		name = node->name;
	}

	printf("%*s+ AST node at %p, type %s, name %s, info %s\n", level*2, "", node, type_, name, info);
	for(child = node->first_child; child; child=child->next_sibling) {
		print_ast(child, level+1);
	}
}
