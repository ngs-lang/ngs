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
	char *type_ = NULL;
	char *name = "(n/a)";
	char info[AST_NODE_INFO_STR_LEN] = "(n/a)";

	switch(node->type) {
		case NUMBER_NODE: type_="number"; snprintf(info, AST_NODE_INFO_STR_LEN-1, "%d", node->number); break;
		case BINOP_NODE: type_="binop"; break;
		case IDENTIFIER_NODE: type_="identifier"; break;
		case ASSIGNMENT_NODE: type_="assignment"; break;
		case EXPRESSIONS_NODE: type_="expressions"; break;
		default:
						 printf("Node type %p %d\n", node, node->type);
						 assert(0=="print_ast(): unknown node type");
	}

	if(node->name) {
		name = node->name;
	}

	DEBUG_PARSER("%*s+ AST node at %p, type %s, name %s, info %s\n", level*2, "", node, type_, name, info);
	for(child = node->first_child; child; child=child->next_sibling) {
		print_ast(child, level+1);
	}
}
