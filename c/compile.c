#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utarray.h>
#include "ngs.h"
#include "ast.h"
#include "parser.h"
#include "vm.h"
#include "compile.h"

#define OPCODE(buf, x) (buf)[*idx]=x; (*idx)++
#define L_STR(buf, x) int l = strlen(x); assert(l<256); OPCODE(buf, l); memcpy((buf)+(*idx), x, l); (*idx) += l;
#define DATA(buf, x) memcpy((buf)+(*idx), &(x), sizeof(x)); (*idx) += sizeof(x)
#define DATA_UINT16(buf, x) *(uint16_t *)&(buf)[*idx] = x; (*idx)+=2

// Symbol table:
// symbol -> [offset1, offset2, ..., offsetN]


void inline ensure_room(char **buf, const size_t cur_len, size_t *allocated, size_t room) {
	size_t new_size;
	DEBUG_COMPILER("entering ensure_room() buf=%p, cur_len=%zu, allocated=%zu, room=%zu\n", *buf, cur_len, *allocated, room);
	assert(*allocated);
	if(*allocated-cur_len >= room) {
		DEBUG_COMPILER("leaving ensure_room() status=enough_room buf=%p, cur_len=%zu, allocated=%zu, room=%zu\n", *buf, cur_len, *allocated, room);
		return;
	}
	for(new_size = *allocated; new_size-cur_len < room; new_size <<= 1) ;
	*buf = NGS_REALLOC(*buf, new_size);
	assert(buf);
	*allocated = new_size;
	DEBUG_COMPILER("leaving ensure_room() status=realloc_done buf=%p, cur_len=%zu, allocated=%zu, room=%zu\n", *buf, cur_len, *allocated, room);
}

SYMBOL_TABLE *get_symbol_table_entry(SYMBOL_TABLE **st, char *name, int create_if_not_exists, int *created) {
	SYMBOL_TABLE *s;
	HASH_FIND_STR(*st, name, s);
	*created = 0;
	if(s) {
		return s;
	}
	if(!create_if_not_exists) {
		return NULL;
	}
	s = NGS_MALLOC(sizeof(*s));
	s->name = strdup(name);
	s->is_predefinded_global = 0;
	HASH_ADD_STR(*st, name /* field */, s);
	*created = 1;
	return s;
}


void compile_main_section(COMPILATION_CONTEXT *ctx, ast_node *node, char **buf, size_t *idx, size_t *allocated) {
	ast_node *ptr;
	int n_args = 0;

	SYMBOL_TABLE *st = NULL;
	int created;
	size_t index;

	ensure_room(buf, *idx, allocated, 1024); // XXX - magic number

	switch(node->type) {
		case BINOP:
			for(ptr=node->first_child, n_args=0; ptr; ptr=ptr->next_sibling, n_args++) {
				compile_main_section(ctx, ptr, buf, idx, allocated);
			}
			OPCODE(*buf, OP_PUSH_INT); DATA(*buf, n_args);
			OPCODE(*buf, OP_FETCH_GLOBAL);
			st = get_symbol_table_entry(&ctx->globals, node->name, 1, &created);
			if(created) {
				int global_found;
				st->index = check_global_index(&ctx->vm, node->name, strlen(node->name), &global_found);
				DEBUG_COMPILER("New global symbol name=%s is_predefinded_global=%d\n", node->name, global_found);
				// global_found = 0; // XXX: test
				st->is_predefinded_global = global_found;
				if(!st->is_predefinded_global) {
					utarray_new(st->offsets, &ut_size_t_icd);
				}
			}
			if(st->is_predefinded_global) {
				index = st->index;
			} else {
				index = 0;
				utarray_push_back(st->offsets, idx);
				DEBUG_COMPILER("Global %s to be patched at offset %d\n", node->name, *idx);
			}
			DATA_UINT16(*buf, index);
			OPCODE(*buf, OP_CALL);
			break;
		case NUMBER:
			/*printf("Compiling NUMBER @ %d\n", *idx);*/
			OPCODE(*buf, OP_PUSH_INT); DATA(*buf, node->number);
			break;
	}
}

#define FOR_GLOBALS(x) \
	for(globals=ctx->globals; globals; globals=globals->hh.next) { \
		if(!globals->is_predefinded_global) { \
			assert(utarray_len(globals->offsets)); /* there must be offsets, otherwise we got some corrupted data */ \
			x \
		} \
	}

size_t calculate_init_section_size(COMPILATION_CONTEXT *ctx) {
	size_t ret = 0;
	SYMBOL_TABLE *globals;
	DEBUG_COMPILER("%s", "entering calculate_init_section_size()\n");
	FOR_GLOBALS(
			// OP_PUSH_L_STR - 1
			// LEN - 1
			// STRING - ?
			// OP_RESOLVE_GLOBAL - 1
			// 1st till Nth-1 offsets: [OP_DUP, OP_PATCH, DATA0, DATA1] - 4
			// Nth offset:             [OP_PATCH, DATA0, DATA1] - 3
			ret += 1 + 1 + strlen(globals->name) + 1 + utarray_len(globals->offsets) * 4 - 1 /* last one has no OP_DUP */;
	)

	ret += 1; // OP_INIT_DONE
	DEBUG_COMPILER("leaving calculate_init_section_size() -> %d\n", ret);
	return ret;
}

void compile_init_section(COMPILATION_CONTEXT *ctx, char **init_buf, size_t *idx) {
	// TODO: check that all offsets are really in UINT16 range.
	char *buf = NULL;
	SYMBOL_TABLE *globals;
	size_t init_section_size;
	DEBUG_COMPILER("%s", "entering compile_init_section()\n");
	init_section_size = calculate_init_section_size(ctx);
	buf = NGS_MALLOC(COMPILE_INITIAL_BUF_SIZE);
	FOR_GLOBALS(
			// OP_PUSH_L_STR - 1
			// LEN - 1
			// STRING - ?
			// OP_RESOLVE_GLOBAL - 1
			// 1st till Nth-1 offsets: [OP_DUP, OP_PATCH, DATA0, DATA1] - 4
			// Nth offset:             [OP_PATCH, DATA0, DATA1] - 3
			size_t i;
			size_t len;
			OPCODE(buf, OP_PUSH_L_STR);
			L_STR(buf, globals->name);
			OPCODE(buf, OP_RESOLVE_GLOBAL);
			len = utarray_len(globals->offsets);
			for(i = 0; i < len; i++) {
				if(i < len - 1) {
					DEBUG_COMPILER("%s", "compile_init_section() dup\n");
					OPCODE(buf, OP_DUP);
				}
				OPCODE(buf, OP_PATCH);
				DEBUG_COMPILER("compile_init_section() global i=%d name=%s offset=%d idx=%d\n", i, globals->name, *(int *)utarray_eltptr(globals->offsets, i), *idx);
				DATA_UINT16(buf, *(int *)utarray_eltptr(globals->offsets, i) + init_section_size - *idx - 2 /* sizeof this uint16 itself */);
			}
	);
	OPCODE(buf, OP_INIT_DONE);
	assert(*idx == init_section_size); // the init section size calculations must be correct
	int i;
	for(i=0; i<*idx; i++) {
		PRINTF_DEBUG(DEBUG_FLAG_BYTECODE, "INIT BYTECODE [%d] %d\n", i, buf[i]);
	}
	DEBUG_COMPILER("%s", "leaving compile_init_section()\n");
	*init_buf = buf;
}

char *compile(ast_node *node /* the top level node */, size_t *len) {

	char *main_buf = NGS_MALLOC(COMPILE_INITIAL_BUF_SIZE);
	size_t main_allocated = COMPILE_INITIAL_BUF_SIZE;
	size_t main_len = 0;
	COMPILATION_CONTEXT ctx;

	char *init_buf;
	size_t init_len = 0;

	char *result_buf;

	vm_init(&(ctx.vm));
	ctx.globals = NULL;

	*len = 0;
	compile_main_section(&ctx, node, &main_buf, &main_len, &main_allocated);
	ensure_room(&main_buf, main_len, &main_allocated, 1);
	main_buf[(main_len)++] = OP_HALT;

	compile_init_section(&ctx, &init_buf, &init_len);

	*len = init_len + main_len;
	result_buf = NGS_MALLOC(*len);
	memcpy(&result_buf[0], init_buf, init_len);
	memcpy(&result_buf[init_len], main_buf, main_len);

	return result_buf;
}
