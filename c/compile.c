#include <assert.h>
#include "ngs.h"
#include "ast.h"
#include "compile.h"
#include "obj.h"

// TODO: abstract UINT16

#define OPCODE(buf, x) { (buf)[*idx]=x; (*idx)++; }
#define L_STR(buf, x) { int l = strlen(x); assert(l<256); OPCODE(buf, l); memcpy((buf)+(*idx), x, l); (*idx) += l; }
#define DATA(buf, x) { memcpy((buf)+(*idx), &(x), sizeof(x)); (*idx) += sizeof(x); }
#define DATA_INT(buf, x) { *(int *)&(buf)[*idx] = x; (*idx)+=sizeof(int); }
#define DATA_UINT16(buf, x) { *(uint16_t *)&(buf)[*idx] = x; (*idx)+=2; }
#define DATA_INT16(buf, x) { *(int16_t *)&(buf)[*idx] = x; (*idx)+=2; }
#define DATA_INT16_AT(buf, loc, x) { *(int16_t *)&(buf)[loc] = x; }
#define DATA_JUMP_OFFSET(buf, x) { *(JUMP_OFFSET *)&(buf)[*idx] = x; (*idx)+=sizeof(JUMP_OFFSET); }
#define DATA_JUMP_OFFSET_PLACEHOLDER(buf) DATA_JUMP_OFFSET(buf, 1024)
#define DATA_PATCH_OFFSET(buf, x) { *(PATCH_OFFSET *)&(buf)[*idx] = x; (*idx)+=sizeof(PATCH_OFFSET); }
#define DATA_N_LOCAL_VARS(buf, x) { *(LOCAL_VAR_INDEX *)&(buf)[*idx] = x; (*idx)+=sizeof(LOCAL_VAR_INDEX); }
#define DATA_N_GLOBAL_VARS(buf, x) { *(GLOBAL_VAR_INDEX *)&(buf)[*idx] = x; (*idx)+=sizeof(GLOBAL_VAR_INDEX); }

// Symbol table:
// symbol -> [offset1, offset2, ..., offsetN]

#define POP_IF_DONT_NEED_RESULT(buf) if(!need_result) OPCODE(buf, OP_POP);
#define DUP_IF_NEED_RESULT(buf) if(need_result) OPCODE(buf, OP_DUP);

#define DONT_NEED_RESULT (0)
#define NEED_RESULT (1)

typedef enum identifier_resolution_type {
	RESOLVE_ANY_IDENTFIER=0,
	RESOLVE_GLOBAL_IDENTFIER=1,
} IDENTIFIER_RESOLUTION_TYPE;

#define LOCALS (ctx->locals[ctx->locals_ptr-1])
#define N_LOCALS (ctx->n_locals[ctx->locals_ptr-1])
// #define IN_FUNCTION (ctx->locals_ptr)

/* static here makes `clang` compiler happy and not "undefined reference to `ensure_room'"*/
static inline void ensure_room(char **buf, const size_t cur_len, size_t *allocated, size_t room) {
	size_t new_size;
	// DEBUG_COMPILER("entering ensure_room() buf=%p, cur_len=%zu, allocated=%zu, room=%zu\n", *buf, cur_len, *allocated, room);
	assert(*allocated);
	if(*allocated-cur_len >= room) {
		// DEBUG_COMPILER("leaving ensure_room() status=enough_room buf=%p, cur_len=%zu, allocated=%zu, room=%zu\n", *buf, cur_len, *allocated, room);
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
	*created = 0;
	HASH_FIND(hh, *st, name, strlen(name), s);
	// printf("SYMBOL TABLE QUERY %s (len %zu) %p\n", name, strlen(name), *st);
	if(s) {
		// printf("SYMBOL TABLE LOOKUP OK %s -> %p\n", name, s);
		return s;
	}
	if(!create_if_not_exists) {
		return NULL;
	}
	s = NGS_MALLOC(sizeof(*s));
	s->name = strdup(name);
	s->is_predefinded_global = 0;
	// HASH_ADD_STR(*st, name /* field */, s);
	HASH_ADD_KEYPTR(hh, *st, s->name, strlen(s->name), s);
	*created = 1;
	// printf("SYMBOL TABLE ADD OK %s -> %p\n", name, *st);
	return s;
}


GLOBAL_VAR_INDEX get_global_var_index(COMPILATION_CONTEXT *ctx, char *name, size_t *idx) {
	SYMBOL_TABLE *st = NULL;
	int created;
	st = get_symbol_table_entry(&ctx->globals, name, 1, &created);
	if(created) {
		int global_found;
		st->index = check_global_index(&ctx->vm, name, strlen(name), &global_found);
		DEBUG_COMPILER("New global symbol name=%s is_predefinded_global=%d\n", name, global_found);
		// global_found = 0; // XXX: test
		st->is_predefinded_global = global_found;
		if(!st->is_predefinded_global) {
			utarray_new(st->offsets, &ut_size_t_icd);
		}
	}
	if(st->is_predefinded_global) {
		return st->index;
	}

	utarray_push_back(st->offsets, idx);
	DEBUG_COMPILER("Global %s to be patched at offset %zu\n", name, *idx);
	return 0;
}

IDENTIFIER_INFO resolve_identifier(COMPILATION_CONTEXT *ctx, char *name, IDENTIFIER_RESOLUTION_TYPE res_type) {
	IDENTIFIER_INFO ret;
	SYMBOL_TABLE *s;

	if(res_type==RESOLVE_ANY_IDENTFIER && ctx->locals_ptr) {
		HASH_FIND(hh, LOCALS, name, strlen(name), s);
		if(s) {
			ret.type = LOCAL_IDENTIFIER;
			ret.index = s->index;
			goto exit;
		}
		// TODO: upvars
	}

	HASH_FIND(hh, ctx->globals, name, strlen(name), s);
	if(s) {
		ret.type = GLOBAL_IDENTIFIER;
		ret.index = s->index;
		goto exit;
	}

	// Identifier not found
	ret.type = NO_IDENTIFIER;

exit:
	return ret;
}

void register_local_var(COMPILATION_CONTEXT *ctx, char *name) {
	SYMBOL_TABLE *s;
	HASH_FIND(hh, LOCALS, name, strlen(name), s);
	if(s) {
		return;
	}
	assert(N_LOCALS < MAX_LOCALS);
	s = NGS_MALLOC(sizeof(*s));
	s->name = strdup(name);
	s->index = N_LOCALS++;
	HASH_ADD_KEYPTR(hh, LOCALS, s->name, strlen(s->name), s);
}

void register_local_vars(COMPILATION_CONTEXT *ctx, ast_node *node) {
	ast_node *ptr;
	switch(node->type) {
		case FUNC_NODE:
			if(node->first_child->next_sibling->next_sibling) {
				// Function has a name
				register_local_var(ctx, node->first_child->next_sibling->next_sibling->name);
			}
			return;
		case ASSIGNMENT_NODE:
		case ASSIGN_DEFAULT_NODE:
			ptr = node->first_child;
			switch(ptr->type) {
				case IDENTIFIER_NODE:
					register_local_var(ctx, ptr->name);
			}
	}
	for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
		register_local_vars(ctx, ptr);
	}
}

void compile_main_section(COMPILATION_CONTEXT *ctx, ast_node *node, char **buf, size_t *idx, size_t *allocated, int need_result) {
	ast_node *ptr;
	int argc = 0;
	GLOBAL_VAR_INDEX index;
	LOCAL_VAR_INDEX n_locals, n_params_required, n_params_optional;
	IDENTIFIER_INFO identifier_info;
	size_t loop_beg, cond_jump, func_jump, end_of_func_idx, if_jump, while_jump;

	ensure_room(buf, *idx, allocated, 1024); // XXX - magic number

	switch(node->type) {
		case CALL_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "CALL NODE", *idx);
			OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
			for(ptr=node->first_child->next_sibling, argc=0; ptr; ptr=ptr->next_sibling, argc++) {
				compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
			}
			OPCODE(*buf, OP_PUSH_INT); DATA(*buf, argc);
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case NUMBER_NODE:
			/*printf("Compiling tNUMBER @ %d\n", *idx);*/
			if(need_result) {
				OPCODE(*buf, OP_PUSH_INT); DATA(*buf, node->number);
			}
			break;
		case IDENTIFIER_NODE:
			identifier_info = resolve_identifier(ctx, node->name, RESOLVE_ANY_IDENTFIER);
			switch(identifier_info.type) {
				case LOCAL_IDENTIFIER:
					OPCODE(*buf, OP_FETCH_LOCAL);
					DATA_N_LOCAL_VARS(*buf, identifier_info.index);
					break;
				case UPVAR_IDENTIFIER:
					assert(0=="Upvars are not implemented yet");
					break;
				case NO_IDENTIFIER:
				case GLOBAL_IDENTIFIER:
					OPCODE(*buf, OP_FETCH_GLOBAL);
					index = get_global_var_index(ctx, node->name, idx);
					DATA_N_GLOBAL_VARS(*buf, index);
					break;
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case ASSIGNMENT_NODE:
			ptr = node->first_child;
			compile_main_section(ctx, ptr->next_sibling, buf, idx, allocated, NEED_RESULT);
			DUP_IF_NEED_RESULT(*buf);
			switch(ptr->type) {
				case IDENTIFIER_NODE:
					// TODO: handle local vs global
					DEBUG_COMPILER("COMPILER: %s %zu\n", "identifier <- expression", *idx);
					identifier_info = resolve_identifier(ctx, ptr->name, RESOLVE_ANY_IDENTFIER);
					switch(identifier_info.type) {
						case LOCAL_IDENTIFIER:
							OPCODE(*buf, OP_STORE_LOCAL);
							// index = get_global_var_index(ctx, ptr->name, idx);
							// DATA_UINT16(*buf, index);
							DATA_N_LOCAL_VARS(*buf, identifier_info.index);
							break;
						case UPVAR_IDENTIFIER:
							assert(0=="Upvars are not implemented yet");
							break;
						case NO_IDENTIFIER:
						case GLOBAL_IDENTIFIER:
							// printf("identifier_info.type %d\n", identifier_info.type);
							OPCODE(*buf, OP_STORE_GLOBAL);
							index = get_global_var_index(ctx, ptr->name, idx);
							DATA_N_GLOBAL_VARS(*buf, index);
							break;
					}
					break;
				default:
					assert(0=="compile_main_section(): assignment to unknown node type");
			}
			break;
		case EXPRESSIONS_NODE:
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				compile_main_section(ctx, ptr, buf, idx, allocated, (ptr == node->last_child) && need_result);
			}
			break;
		case FOR_NODE:
			// setup
			compile_main_section(ctx, node->first_child, buf, idx, allocated, DONT_NEED_RESULT);
			// condition
			loop_beg = *idx;
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			cond_jump = *idx;
			OPCODE(*buf, OP_JMP_FALSE);
			DATA_JUMP_OFFSET(*buf, 1024);
			// body
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling->next_sibling, buf, idx, allocated, DONT_NEED_RESULT);
			// increment
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, DONT_NEED_RESULT);
			assert(*idx - cond_jump < 0x7FFF);
			*(JUMP_OFFSET *)&(*buf)[cond_jump+1] = *idx - cond_jump;
			OPCODE(*buf, OP_JMP);
			assert((*idx - loop_beg) < 0x7FFF);
			DATA_JUMP_OFFSET(*buf, -(*idx - loop_beg + sizeof(JUMP_OFFSET)));
			if(need_result) OPCODE(*buf, OP_PUSH_NULL);
			break;
		case EMPTY_NODE:
			break;
		case ARR_LIT_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "ARRAY NODE", *idx);
			for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
				compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
			}
			OPCODE(*buf, OP_PUSH_INT);
			DATA_INT(*buf, argc);
			OPCODE(*buf, OP_MAKE_ARR);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case FUNC_NODE:
			/* Work in progress */
			// FUNC_NODE children: arguments, body
			DEBUG_COMPILER("COMPILER: %s %zu\n", "FUNC NODE", *idx);
			OPCODE(*buf, OP_JMP);
			func_jump = *idx;
			DATA_JUMP_OFFSET(*buf, 0);
			ctx->locals_ptr++;
			assert(ctx->locals_ptr < COMPILE_MAX_FUNC_DEPTH);
			LOCALS = NULL;
			N_LOCALS = 0;
			// Arguments
			for(ptr=node->first_child->first_child; ptr; ptr=ptr->next_sibling) {
				// ptr: identifier, type, default value
				register_local_var(ctx, ptr->first_child->name);
			}
			// Body
			register_local_vars(ctx, node->first_child->next_sibling);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			n_locals = N_LOCALS;
			ctx->locals_ptr--;
			OPCODE(*buf, OP_RET);
			end_of_func_idx = *idx;

			// Arguments' types and default values
			for(ptr=node->first_child->first_child, n_params_required=0; ptr; ptr=ptr->next_sibling) {
				// ptr: identifier, type, default value
				OPCODE(*buf, OP_PUSH_L_STR);
				L_STR(*buf, ptr->first_child->name);
				compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
				n_params_required++;
			}

			n_params_optional = 0; // Not implemented yet

			OPCODE(*buf, OP_PUSH_INT); // XXX. TODO: Add OP_PUSH_N_PARAMS
			DATA_INT(*buf, n_params_required);
			OPCODE(*buf, OP_PUSH_INT); // XXX. TODO: Add OP_PUSH_N_PARAMS
			DATA_INT(*buf, n_params_optional);

			*(JUMP_OFFSET *)&(*buf)[func_jump] = (end_of_func_idx - func_jump - sizeof(JUMP_OFFSET));
			OPCODE(*buf, OP_MAKE_CLOSURE);
			DATA_JUMP_OFFSET(*buf, -(*idx - func_jump + sizeof(LOCAL_VAR_INDEX)));
			DATA_N_LOCAL_VARS(*buf, n_locals);

			if(node->first_child->next_sibling->next_sibling) {
				// Function has a name
				identifier_info = resolve_identifier(ctx, node->first_child->next_sibling->next_sibling->name, RESOLVE_ANY_IDENTFIER);
				switch(identifier_info.type) {
					case LOCAL_IDENTIFIER:
						OPCODE(*buf, OP_DEF_LOCAL_FUNC);
						DATA_N_LOCAL_VARS(*buf, identifier_info.index);
						break;
					case UPVAR_IDENTIFIER:
						assert(0=="Upvars are not implemented yet");
						break;
					case NO_IDENTIFIER:
					case GLOBAL_IDENTIFIER:
						OPCODE(*buf, OP_DEF_GLOBAL_FUNC);
						index = get_global_var_index(ctx, node->first_child->next_sibling->next_sibling->name, idx);
						DATA_N_GLOBAL_VARS(*buf, index);
						break;
				}
			}
			break;
		case STR_COMPS_NODE:
			for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
				if(ptr->type != STR_COMP_IMM_NODE) {
					OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value of OP_TO_STR
				}
				compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
				if(ptr->type != STR_COMP_IMM_NODE) {
					OPCODE(*buf, OP_TO_STR);
				}
			}
			switch(argc) {
				case 0:
					OPCODE(*buf, OP_PUSH_EMPTY_STR);
					break;
				case 1:
					break;
				default:
					OPCODE(*buf, OP_PUSH_INT);
					DATA_INT(*buf, argc);
					OPCODE(*buf, OP_MAKE_STR);
			}
			break;
		case STR_COMP_IMM_NODE:
			OPCODE(*buf, OP_PUSH_L_STR);
			L_STR(*buf, node->name);
			break;
		case NULL_NODE:  OPCODE(*buf, OP_PUSH_NULL); break;
		case TRUE_NODE:  OPCODE(*buf, OP_PUSH_TRUE); break;
		case FALSE_NODE: OPCODE(*buf, OP_PUSH_FALSE); break;
		case DEFINED_NODE:
			identifier_info = resolve_identifier(ctx, node->first_child->name, RESOLVE_ANY_IDENTFIER);
			switch(identifier_info.type) {
				case LOCAL_IDENTIFIER:
					OPCODE(*buf, OP_LOCAL_DEF_P);
					DATA_N_LOCAL_VARS(*buf, identifier_info.index);
					break;
				case UPVAR_IDENTIFIER:
					assert(0=="Upvars are not implemented yet");
					break;
				case NO_IDENTIFIER:
				case GLOBAL_IDENTIFIER:
					OPCODE(*buf, OP_GLOBAL_DEF_P);
					index = get_global_var_index(ctx, node->first_child->name, idx);
					DATA_N_GLOBAL_VARS(*buf, index);
					break;
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case IF_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			if_jump = *idx;
			OPCODE(*buf, OP_JMP_FALSE);
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, need_result);
			OPCODE(*buf, OP_JMP);
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET); // Jump is OP_JMP_FALSE JUMP_OFFSET shorter
			if_jump = *idx - 1 - sizeof(JUMP_OFFSET);
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, need_result);
			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET);
			break;
		case WHILE_NODE:
			loop_beg = *idx;
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			while_jump = *idx;
			OPCODE(*buf, OP_JMP_FALSE);
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, need_result);
			OPCODE(*buf, OP_JMP);
			DATA_JUMP_OFFSET(*buf, -(*idx - loop_beg + sizeof(JUMP_OFFSET)));
			*(JUMP_OFFSET *)&(*buf)[while_jump+1] = *idx - while_jump - 1 - sizeof(JUMP_OFFSET);
			break;
		default:
			assert(0=="compile_main_section(): unknown node type");
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
	DEBUG_COMPILER("leaving calculate_init_section_size() -> %zu\n", ret);
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
				DEBUG_COMPILER("compile_init_section() global i=%zu name=%s offset=%d idx=%zu\n", i, globals->name, *(int *)utarray_eltptr(globals->offsets, i), *idx);
				DATA_PATCH_OFFSET(buf, *(int *)utarray_eltptr(globals->offsets, i) + init_section_size - *idx - sizeof(PATCH_OFFSET));
			}
	);
	OPCODE(buf, OP_INIT_DONE);
	assert(*idx == init_section_size); // the init section size calculations must be correct
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
	ctx.locals = NGS_MALLOC(COMPILE_MAX_FUNC_DEPTH * sizeof(SYMBOL_TABLE *));
	ctx.n_locals = NGS_MALLOC(COMPILE_MAX_FUNC_DEPTH * sizeof(LOCAL_VAR_INDEX *));
	ctx.locals_ptr = 0;
	// ctx.n_locals = 0;
	ctx.in_function = 0;

	*len = 0;
	compile_main_section(&ctx, node, &main_buf, &main_len, &main_allocated, NEED_RESULT);
	ensure_room(&main_buf, main_len, &main_allocated, 1);
	main_buf[(main_len)++] = OP_HALT;

	compile_init_section(&ctx, &init_buf, &init_len);

	*len = init_len + main_len;
	result_buf = NGS_MALLOC(*len);
	memcpy(&result_buf[0], init_buf, init_len);
	memcpy(&result_buf[init_len], main_buf, main_len);

	return result_buf;
}
#undef LOCALS
