#define NGS_COMPILE_BUF_SIZE (65536)
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ngs.h"
#include "ast.h"
#include "parser.h"
#include "vm.h"

#define OPCODE(x) buf[*idx]=x; (*idx)++
#define L_STR(x) int l = strlen(x); assert(l<256); OPCODE(l); memcpy(buf+(*idx), x, l); (*idx) += l;
#define DATA(x) memcpy(buf+(*idx), &(x), sizeof(x)); (*idx) += sizeof(x)
#define DATA_UINT16(x) *(uint16_t *)&buf[*idx] = x; (*idx)+=2

void compile_to_buf(ast_node *node, char *buf, size_t *idx, size_t limit) {
	ast_node *ptr;
	int n_args = 0;
	switch(node->type) {
		case BINOP:
			for(ptr=node->first_child, n_args=0; ptr; ptr=ptr->next_sibling, n_args++) {
				compile_to_buf(ptr, buf, idx, limit);
			}
			OPCODE(OP_PUSH_INT); DATA(n_args);
			OPCODE(OP_FETCH_GLOBAL); DATA_UINT16(0); // XXX: put actual symbol index here
			OPCODE(OP_CALL);
			break;
		case NUMBER:
			/*printf("Compiling NUMBER @ %d\n", *idx);*/
			OPCODE(OP_PUSH_INT); DATA(node->number);
			break;
	}
}

char *compile(ast_node *node, size_t *len) {
	char *buf = NGS_MALLOC(NGS_COMPILE_BUF_SIZE);
	*len = 0;
	compile_to_buf(node, buf, len, NGS_COMPILE_BUF_SIZE);
	buf[(*len)++] = OP_HALT;
	printf("LEN %d\n", *len);
	return buf;
}
