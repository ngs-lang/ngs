#include <assert.h>
#include "ngs.h"
#include "ast.h"
#include "compile.h"
#include "obj.h"

// TODO: disabllow break/continue outside of loops

// TODO: abstract UINT16
//
// TODO: Use exceptions instead of asserts

#define OPCODE(buf, x) { (buf)[*idx]=x; (*idx)++; }
#define L8_STR(buf, x) { int l = strlen(x); assert(l<256); OPCODE(buf, l); memcpy((buf)+(*idx), x, l); (*idx) += l; }
#define DATA(buf, x) { memcpy((buf)+(*idx), &(x), sizeof(x)); (*idx) += sizeof(x); }
#define DATA_INT32(buf, x) { *(int32_t *)&(buf)[*idx] = x; (*idx)+=sizeof(int32_t); }
#define DATA_INT64(buf, x) { *(int64_t *)&(buf)[*idx] = x; (*idx)+=sizeof(int64_t); }
#define DATA_UINT16(buf, x) { *(uint16_t *)&(buf)[*idx] = x; (*idx)+=sizeof(uint16_t); }
#define DATA_UINT32(buf, x) { *(uint32_t *)&(buf)[*idx] = x; (*idx)+=sizeof(uint32_t); }
#define DATA_JUMP_OFFSET(buf, x) { *(JUMP_OFFSET *)&(buf)[*idx] = x; (*idx)+=sizeof(JUMP_OFFSET); }
#define DATA_JUMP_OFFSET_PLACEHOLDER(buf) DATA_JUMP_OFFSET(buf, 1024)
#define DATA_PATCH_OFFSET(buf, x) { *(PATCH_OFFSET *)&(buf)[*idx] = x; (*idx)+=sizeof(PATCH_OFFSET); }
#define DATA_N_LOCAL_VARS(buf, x) { *(LOCAL_VAR_INDEX *)&(buf)[*idx] = x; (*idx)+=sizeof(LOCAL_VAR_INDEX); }
#define DATA_N_GLOBAL_VARS(buf, x) { *(GLOBAL_VAR_INDEX *)&(buf)[*idx] = x; (*idx)+=sizeof(GLOBAL_VAR_INDEX); }
#define DATA_N_UPVAR_INDEX(buf, x) { *(UPVAR_INDEX *)&(buf)[*idx] = x; (*idx)+=sizeof(UPVAR_INDEX); }

// Symbol table:
// symbol -> [offset1, offset2, ..., offsetN]

#define POP_IF_DONT_NEED_RESULT(buf) if(!need_result) OPCODE(buf, OP_POP);
#define DUP_IF_NEED_RESULT(buf) if(need_result) OPCODE(buf, OP_DUP);

#define DONT_NEED_RESULT (0)
#define NEED_RESULT (1)

#define IF_NOT_SWITCH_COND if(node->number != SWITCH_NODE_COND && node->number != SWITCH_NODE_ECOND)

#define LOCALS (ctx->locals[ctx->locals_ptr-1])
#define IDENTIFIERS_SCOPES (ctx->identifiers_scopes[ctx->locals_ptr-1])
#define N_LOCALS (ctx->n_locals[ctx->locals_ptr-1])
#define N_UPLEVELS (ctx->n_uplevels[ctx->locals_ptr-1])
#define STACK_DEPTH (ctx->stack_depth[ctx->locals_ptr])
// #define IN_FUNCTION (ctx->locals_ptr)

#define BREAK_ADDR (ctx->fill_in_break_addrs[ctx->fill_in_break_addrs_ptr-1])
#define CONTINUE_ADDR (ctx->fill_in_continue_addrs[ctx->fill_in_continue_addrs_ptr-1])
#define SETUP_ADDRESS_FILLING() \
			old_break_addrs_ptr = ctx->fill_in_break_addrs_ptr; \
			old_continue_addrs_ptr = ctx->fill_in_continue_addrs_ptr;
#define HANDLE_ADDRESS_FILLING() \
			while(ctx->fill_in_break_addrs_ptr > old_break_addrs_ptr) { \
				*(JUMP_OFFSET *)&(*buf)[BREAK_ADDR] = *idx - (BREAK_ADDR + sizeof(JUMP_OFFSET)) ; \
				ctx->fill_in_break_addrs_ptr--; \
			} \
			while(ctx->fill_in_continue_addrs_ptr > old_continue_addrs_ptr) { \
				*(JUMP_OFFSET *)&(*buf)[CONTINUE_ADDR] = continue_target_idx - (CONTINUE_ADDR + sizeof(JUMP_OFFSET)); \
				ctx->fill_in_continue_addrs_ptr--; \
			}

void compile_main_section(COMPILATION_CONTEXT *ctx, ast_node *node, char **buf, size_t *idx, size_t *allocated, int need_result);

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
	s->name = ngs_strdup(name);
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

IDENTIFIER_INFO resolve_identifier(COMPILATION_CONTEXT *ctx, const char *name) {
	IDENTIFIER_INFO ret;
	SYMBOL_TABLE *s;
	int locals_idx;

	if(ctx->locals_ptr) {
		HASH_FIND(hh, LOCALS, name, strlen(name), s);
		if(s) {
			// printf("xxx-1-1 %s\n", name);
			ret.type = LOCAL_IDENTIFIER;
			ret.index = s->index;
			goto exit;
		}
		// printf("xxx-1-2 %s\n", name);
		for(locals_idx = ctx->locals_ptr-1; locals_idx > 0; locals_idx--) {
			HASH_FIND(hh, ctx->locals[locals_idx-1], name, strlen(name), s);
			if(s) {
				// printf("xxx-2 %s\n", name);
				UPVAR_INDEX u = locals_idx;
				ret.type = UPVAR_IDENTIFIER;
				ret.index = s->index;
				ret.uplevel = ctx->locals_ptr - locals_idx - 1; // -1 is to make it zero based
				// F level1(x) {    -> uplevels=0
				//   F level2() {   -> uplevels=1
				//     F level3() { -> uplevels=2
				//       dump(x)    -> uplevels=2 (we are here, maximize also uplevels for level3, level2 and up)
				//     }
				//   }
				// }
				for(locals_idx = ctx->locals_ptr; locals_idx > 0; locals_idx--) {
					if(ctx->n_uplevels[locals_idx-1] < locals_idx - u) {
						ctx->n_uplevels[locals_idx-1] = locals_idx - u;
					}
					// XXX: else break? all upper levels should be up to date in this case
				}
				goto exit;
			}

		}
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

#define REGISTER_IDENTIFIER(t) \
	SYMBOL_TABLE *s; \
	s = NGS_MALLOC(sizeof(*s)); \
	s->name = ngs_strdup(name); \
	s->is_predefinded_global = 0; \
	s->index = t; \
	HASH_ADD_KEYPTR(hh, IDENTIFIERS_SCOPES, s->name, strlen(s->name), s); \

void register_local_identifier(COMPILATION_CONTEXT *ctx, char *name) { REGISTER_IDENTIFIER(LOCAL_IDENTIFIER); }
void register_upvar_identifier(COMPILATION_CONTEXT *ctx, char *name) { REGISTER_IDENTIFIER(UPVAR_IDENTIFIER); }
void register_global_identifier(COMPILATION_CONTEXT *ctx, char *name) { REGISTER_IDENTIFIER(GLOBAL_IDENTIFIER); }

void register_local_var(COMPILATION_CONTEXT *ctx, char *name) {
	SYMBOL_TABLE *s;
	HASH_FIND(hh, LOCALS, name, strlen(name), s);
	if(s) {
		return;
	}
	assert(N_LOCALS < MAX_LOCALS);
	s = NGS_MALLOC(sizeof(*s));
	s->name = ngs_strdup(name);
	s->index = N_LOCALS++;
	HASH_ADD_KEYPTR(hh, LOCALS, s->name, strlen(s->name), s);
	register_local_identifier(ctx, s->name);
}

// TODO: maybe do it at parse time? That might be more complex but probably faster
// TODO: refactor for code deduplication
void register_local_vars(COMPILATION_CONTEXT *ctx, ast_node *node) {
	SYMBOL_TABLE *s;
	ast_node *ptr, *ptr2;
	switch(node->type) {
		case FUNC_NODE:
			if(node->first_child->next_sibling->next_sibling->next_sibling) {
				// Function has a name
				HASH_FIND(hh, IDENTIFIERS_SCOPES, node->first_child->next_sibling->next_sibling->next_sibling->name, strlen(node->first_child->next_sibling->next_sibling->next_sibling->name), s);
				if(!s) {
					register_local_var(ctx, node->first_child->next_sibling->next_sibling->next_sibling->name);
				}
			}
			return;
		case LOCAL_NODE:
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				switch(ptr->type) {
					case IDENTIFIER_NODE:
						register_local_var(ctx, ptr->name);
						break;
					case ASSIGNMENT_NODE:
						assert(ptr->first_child->type == IDENTIFIER_NODE);
						register_local_var(ctx, ptr->first_child->name);
						for(ptr2=ptr->first_child->next_sibling; ptr2; ptr2=ptr2->next_sibling) {
							register_local_vars(ctx, ptr2);
						}
						break;
					default:
						assert(0 == "Unexpected node type under LOCAL_NODE");
				}
			}
			break;
		case UPVAR_NODE:
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				switch(ptr->type) {
					case IDENTIFIER_NODE:
						register_upvar_identifier(ctx, ptr->name);
						break;
					case ASSIGNMENT_NODE:
						assert(ptr->first_child->type == IDENTIFIER_NODE);
						register_upvar_identifier(ctx, ptr->first_child->name);
						for(ptr2=ptr->first_child->next_sibling; ptr2; ptr2=ptr2->next_sibling) {
							register_local_vars(ctx, ptr2);
						}
						break;
					default:
						assert(0 == "Unexpected node type under UPVAR_NODE");
				}
			}
			break;
		case GLOBAL_NODE:
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				switch(ptr->type) {
					case IDENTIFIER_NODE:
						// printf("REG GLOB %s\n", ptr->name);
						register_global_identifier(ctx, ptr->name);
						break;
					case ASSIGNMENT_NODE:
						assert(ptr->first_child->type == IDENTIFIER_NODE);
						register_global_identifier(ctx, ptr->first_child->name);
						for(ptr2=ptr->first_child->next_sibling; ptr2; ptr2=ptr2->next_sibling) {
							register_local_vars(ctx, ptr2);
						}
						break;
					default:
						assert(0 == "Unexpected node type under GLOBAL_NODE");
				}
			}
			break;
		case ASSIGNMENT_NODE:
			if(node->first_child->type == IDENTIFIER_NODE) {
				HASH_FIND(hh, IDENTIFIERS_SCOPES, node->first_child->name, strlen(node->first_child->name), s);
				if(!s) {
					IDENTIFIER_INFO identifier_info;
					identifier_info = resolve_identifier(ctx, node->first_child->name);
					if(identifier_info.type == UPVAR_IDENTIFIER) {
						s = NGS_MALLOC(sizeof(*s));
						s->name = ngs_strdup(node->first_child->name);
						s->is_predefinded_global = 0;
						s->index = UPVAR_IDENTIFIER;
						HASH_ADD_KEYPTR(hh, IDENTIFIERS_SCOPES, s->name, strlen(s->name), s);
					}
				}
				if(!s) {
					register_local_var(ctx, node->first_child->name);
				}
			}
			for(ptr=node->first_child->next_sibling; ptr; ptr=ptr->next_sibling) {
				register_local_vars(ctx, ptr);
			}
			break;
	}
	for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
		register_local_vars(ctx, ptr);
	}
}

void compile_identifier(COMPILATION_CONTEXT *ctx, char **buf, size_t *idx, char *name, int opcode_local, int opcode_upvar, int opcode_global) {
	IDENTIFIER_INFO identifier_info;
	GLOBAL_VAR_INDEX gvi;
	identifier_info = resolve_identifier(ctx, name);
	// printf("NAME %s\n", name);
	switch(identifier_info.type) {
		case LOCAL_IDENTIFIER:
			// printf(" LOCAL\n");
			OPCODE(*buf, opcode_local);
			DATA_N_LOCAL_VARS(*buf, identifier_info.index);
			break;
		case UPVAR_IDENTIFIER:
			// printf(" UPVAR\n");
			OPCODE(*buf, opcode_upvar);
			DATA_N_UPVAR_INDEX(*buf, identifier_info.uplevel);
			DATA_N_LOCAL_VARS(*buf, identifier_info.index);
			break;
		case NO_IDENTIFIER:
		case GLOBAL_IDENTIFIER:
			// printf(" GLOBAL\n");
			OPCODE(*buf, opcode_global);
			gvi = get_global_var_index(ctx, name, idx);
			DATA_N_GLOBAL_VARS(*buf, gvi);
			break;
	}
}

void compile_attr_name(COMPILATION_CONTEXT *ctx, ast_node *node, char **buf, size_t *idx, size_t *allocated, int need_result) {
	if(!need_result) {
		return;
	}
	if(node->type != IDENTIFIER_NODE) {
		compile_main_section(ctx, node, buf, idx, allocated, NEED_RESULT);
		return;
	}
	OPCODE(*buf, OP_PUSH_L8_STR);
	L8_STR(*buf, node->name);
}

void compile_main_section(COMPILATION_CONTEXT *ctx, ast_node *node, char **buf, size_t *idx, size_t *allocated, int need_result) {
	ast_node *ptr, *callable;
	int argc, have_arr_splat, have_hash_splat, params_flags;
	int doing_named_args = 0;
	LOCAL_VAR_INDEX n_locals, n_params_required, n_params_optional;
	UPVAR_INDEX n_uplevels;
	size_t loop_beg_idx, cond_jump, continue_target_idx, func_jump, end_of_func_idx, if_jump, while_jump;
	int old_break_addrs_ptr, old_continue_addrs_ptr, i, saved_stack_depth;
	SYMBOL_TABLE *st;

	// Can probably be overwhelmed by number of local variables with lengthy names
	ensure_room(buf, *idx, allocated, 1024); // XXX - magic number

	// printf("compile_main_section() node=%p type=%s last_child=%p need_result=%d\n", node, NGS_AST_NODE_TYPES_NAMES[node->type], node->last_child, need_result);
	if(node->location.first_line) {
		source_tracking_entry *ste = NULL;
		// printf("LOC: ip=%lu\n %d:%d %d:%d\n", *idx, node->location.first_line, node->location.first_column, node->location.last_line, node->location.last_column);
		if(ctx->source_tracking_entries_count) {
			if(ctx->source_tracking_entries[ctx->source_tracking_entries_count-1].ip == *idx) {
				// Override because deeper ast nodes have more precise location
				ste = &ctx->source_tracking_entries[ctx->source_tracking_entries_count-1];
			} else {
				ste = NULL;
			}
		}
		if(!ste) {
			if (ctx->source_tracking_entries_count == ctx->source_tracking_entries_allocated) {
				ctx->source_tracking_entries_allocated *= 2;
				ctx->source_tracking_entries = NGS_REALLOC(ctx->source_tracking_entries, ctx->source_tracking_entries_allocated * sizeof(source_tracking_entry));
			}
			ste = &ctx->source_tracking_entries[ctx->source_tracking_entries_count++];
		}
		ste->source_file_name_idx = 0; // XXX: currently only one source file per compile() is supported
		ste->ip = *idx;
		ste->source_location[0] = node->location.first_line;
		ste->source_location[1] = node->location.first_column;
		ste->source_location[2] = node->location.last_line;
		ste->source_location[3] = node->location.last_column;
	}
	switch(node->type) {
		case CALL_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "CALL NODE", *idx);
			OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
			saved_stack_depth = STACK_DEPTH;
			STACK_DEPTH++;
			// print_ast(node, 0);
			assert(node->first_child->next_sibling->type == ARGS_NODE);
			for(ptr=node->first_child->next_sibling->first_child, have_arr_splat=0, have_hash_splat=0; ptr; ptr=ptr->next_sibling) {
				assert(ptr->type == ARG_NODE);
				if(ptr->first_child->type == ARR_SPLAT_NODE) {
					have_arr_splat = 1;
				}
				if(ptr->first_child->type == HASH_SPLAT_NODE) {
					have_hash_splat = 1;
				}
			}
			if(have_arr_splat) {
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
				OPCODE(*buf, OP_MAKE_ARR);
				STACK_DEPTH++;
			}
			doing_named_args = 0;
			argc = 0;
			if(node->first_child->type == ATTR_NODE) {
				compile_main_section(ctx, node->first_child->first_child, buf, idx, allocated, NEED_RESULT);
				if(have_arr_splat) {
					OPCODE(*buf, OP_ARR_APPEND);
				}
				argc++;
			}
			for(ptr=node->first_child->next_sibling->first_child; ptr; ptr=ptr->next_sibling) {
				assert(ptr->type == ARG_NODE);
				if(ptr->first_child->next_sibling) {
					// Got named argument
					if(!doing_named_args) {
						// Setup named arguments
						doing_named_args = 1;
						// TODO: maybe special opcode for creating an empty hash?
						OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
						OPCODE(*buf, OP_MAKE_HASH);
						STACK_DEPTH++;
						argc++;
					}
					// argument name
					compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
					// argument value
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_HASH_SET);
					continue;
				}
				if(ptr->first_child->type == ARR_SPLAT_NODE) {
					assert(!doing_named_args);
					compile_main_section(ctx, ptr->first_child->first_child, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_TO_ARR);
					OPCODE(*buf, OP_ARR_CONCAT);
					continue;
				}
				if(ptr->first_child->type == HASH_SPLAT_NODE) {
					if(!doing_named_args) {
						// Setup named arguments
						doing_named_args = 1;
						// TODO: maybe special opcode for creating an empty hash?
						OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
						OPCODE(*buf, OP_MAKE_HASH);
						// XXX needed? /// STACK_DEPTH++;
						argc++;
					}
					compile_main_section(ctx, ptr->first_child->first_child, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_HASH_UPDATE);
					continue;
				}
				assert(!doing_named_args);
				compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
				STACK_DEPTH++;
				argc++;
				if(have_arr_splat) {
					OPCODE(*buf, ptr->first_child->type == ARR_SPLAT_NODE ? OP_ARR_CONCAT : OP_ARR_APPEND);
				}
			}
			if(doing_named_args) {
				// Marker at the end
				OPCODE(*buf, OP_PUSH_KWARGS_MARKER);
				argc++;
				if(have_arr_splat) {
					OPCODE(*buf, OP_ARR_APPEND2);
				}
			}
			if(!have_arr_splat) {
				assert(argc <= MAX_ARGS); // TODO: Exception
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, argc);
				STACK_DEPTH++;
			}
			if(node->first_child->type == ATTR_NODE) {
				callable = node->first_child->first_child->next_sibling;
			} else {
				callable = node->first_child;
			}
			compile_main_section(ctx, callable, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, have_arr_splat ? OP_CALL_ARR : OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);
			STACK_DEPTH = saved_stack_depth;
			break;
		case INDEX_NODE:
		case ATTR_NODE:
		case NS_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "INDEX NODE", *idx);
			OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
			saved_stack_depth = STACK_DEPTH;
			STACK_DEPTH++;
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			STACK_DEPTH++;
			if(node->type == ATTR_NODE || node->type == NS_NODE) {
				compile_attr_name(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			} else {
				compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			}
			STACK_DEPTH++;
			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 2);
			STACK_DEPTH++;
			compile_identifier(ctx, buf, idx, node->type == INDEX_NODE ? "[]" : (node->type == NS_NODE ? "::" : "."), OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
			OPCODE(*buf, OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);
			STACK_DEPTH = saved_stack_depth;
			break;
		case INT_NODE:
			/*printf("Compiling tNUMBER @ %d\n", *idx);*/
			if(need_result) {
				// (>> TAG_BITS) is workaround for highest bits of OP_PUSH_INT32 get lost
				if(node->number < (INT32_MIN >> TAG_BITS) || node->number > (INT32_MAX >> TAG_BITS)) {
					OPCODE(*buf, OP_PUSH_INT64); DATA_INT64(*buf, node->number);
				} else {
					int n = (int) node->number;
					OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, n);
				}
			}
			break;
		case REAL_NODE:
			if(need_result) {
				OPCODE(*buf, OP_PUSH_REAL); DATA(*buf, *(NGS_REAL *)node->data);
			}
			break;
		case IDENTIFIER_NODE:
			compile_identifier(ctx, buf, idx, node->name, OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case ASSIGNMENT_NODE:
			ptr = node->first_child;
			switch(ptr->type) {
				case IDENTIFIER_NODE:
					DEBUG_COMPILER("COMPILER: %s %zu\n", "identifier <- expression", *idx);
					compile_main_section(ctx, ptr->next_sibling, buf, idx, allocated, NEED_RESULT);
					DUP_IF_NEED_RESULT(*buf);
					compile_identifier(ctx, buf, idx, ptr->name, OP_STORE_LOCAL, OP_STORE_UPVAR, OP_STORE_GLOBAL);
					break;
				case INDEX_NODE:
					OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
					compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 3);
					compile_identifier(ctx, buf, idx, "[]=", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
					OPCODE(*buf, OP_CALL);
					POP_IF_DONT_NEED_RESULT(*buf);
					break;
				case ATTR_NODE:
				case NS_NODE:
					OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					compile_attr_name(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
					compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 3);
					compile_identifier(ctx, buf, idx, ptr->type == ATTR_NODE ? ".=" : "::=", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
					OPCODE(*buf, OP_CALL);
					POP_IF_DONT_NEED_RESULT(*buf);
					break;
				default:
					assert(0=="compile_main_section(): assignment to unknown node type");
			}
			break;
		case EXPRESSIONS_NODE:
			if(!node->first_child && need_result) {
				OPCODE(*buf, OP_PUSH_NULL);
				break;
			}
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				// printf("EXPRESSIONS_NODE ptr=%p type=%s need_result=%d will_do_result=%d\n", ptr, NGS_AST_NODE_TYPES_NAMES[ptr->type], need_result, (ptr == node->last_child) && need_result);
				compile_main_section(ctx, ptr, buf, idx, allocated, (!ptr->next_sibling) && need_result);
			}
			break;
		case FOR_NODE:
			// setup
			compile_main_section(ctx, node->first_child, buf, idx, allocated, DONT_NEED_RESULT);
			// condition
			loop_beg_idx = *idx;
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_JMP_FALSE);
			cond_jump = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			// body
			SETUP_ADDRESS_FILLING();
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling->next_sibling, buf, idx, allocated, DONT_NEED_RESULT);
			// increment
			continue_target_idx = *idx;
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, DONT_NEED_RESULT);
			// jump to condition
			OPCODE(*buf, OP_JMP);
			assert(*idx - cond_jump < 0x7FFF);
			*(JUMP_OFFSET *)&(*buf)[cond_jump] = *idx - cond_jump;
			assert((*idx - loop_beg_idx) < 0x7FFF);
			DATA_JUMP_OFFSET(*buf, -(*idx - loop_beg_idx + sizeof(JUMP_OFFSET)));
			HANDLE_ADDRESS_FILLING();
			if(need_result) OPCODE(*buf, OP_PUSH_NULL);
			break;
		case EMPTY_NODE:
			break;
		case ARR_LIT_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "ARRAY NODE", *idx);
			for(ptr=node->first_child, have_arr_splat=0; ptr; ptr=ptr->next_sibling) {
				if(ptr->type == ARR_SPLAT_NODE) {
					have_arr_splat = 1;
					break;
				}
			}
			if(have_arr_splat) {
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
				OPCODE(*buf, OP_MAKE_ARR);
			}
			for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
				if(ptr->type == ARR_SPLAT_NODE) {
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_TO_ARR);
				} else {
					compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
				}
				if(have_arr_splat) {
					OPCODE(*buf, ptr->type == ARR_SPLAT_NODE ? OP_ARR_CONCAT : OP_ARR_APPEND);
				}
			}
			if(!have_arr_splat) {
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, argc);
				OPCODE(*buf, OP_MAKE_ARR);
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case FUNC_NODE:
			// FUNC_NODE children: arguments, body
			DEBUG_COMPILER("COMPILER: %s %zu\n", "FUNC NODE", *idx);
			OPCODE(*buf, OP_JMP);
			func_jump = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			ctx->locals_ptr++;
			assert(ctx->locals_ptr < COMPILE_MAX_FUNC_DEPTH);
			LOCALS = NULL;
			IDENTIFIERS_SCOPES = NULL;
			N_LOCALS = 0;
			N_UPLEVELS = 0;
			STACK_DEPTH = 0;
			params_flags = 0;
			// Arguments
			for(ptr=node->first_child->first_child; ptr; ptr=ptr->next_sibling) {
				// ptr children: identifier, type, (default value | splat indicator)
				register_local_var(ctx, ptr->first_child->name);
			}
			// Body
			register_local_vars(ctx, node->first_child->next_sibling);
			// if(N_UPLEVELS) printf("UPLEVELS %d\n", N_UPLEVELS);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_RET);
			end_of_func_idx = *idx;

			n_locals = N_LOCALS;
			n_uplevels = N_UPLEVELS;

			// Local variables names
			// TODO: something more efficient
			for(st=LOCALS; st; st=st->hh.next) {
				OPCODE(*buf, OP_PUSH_L8_STR);
				L8_STR(*buf, st->name);
			}

			ctx->locals_ptr--;

			// Arguments' types and default values
			for(ptr=node->first_child->first_child, n_params_required=0, n_params_optional=0; ptr; ptr=ptr->next_sibling) {
				// ptr children: identifier, type, (default value | splat indicator)
				OPCODE(*buf, OP_PUSH_L8_STR);
				L8_STR(*buf, ptr->first_child->name);
				// printf("PT 0 %s\n", ptr->first_child->name);
				compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
				if(ptr->first_child->next_sibling->next_sibling) {
					// Either array/hash splat or default value
					if(ptr->first_child->next_sibling->next_sibling->type == ARR_SPLAT_NODE) {
						if(ptr->next_sibling && (ptr->next_sibling->first_child->next_sibling->next_sibling->type != HASH_SPLAT_NODE)) {
							assert(0 == "splat function parameter must be the last one or followed by keyword splat only");
						}
						params_flags |= PARAMS_FLAG_ARR_SPLAT;
						continue;
					}
					if(ptr->first_child->next_sibling->next_sibling->type == HASH_SPLAT_NODE) {
						if(ptr->next_sibling) {
							assert(0 == "keyword splat function parameter must be the last one");
						}
						params_flags |= PARAMS_FLAG_HASH_SPLAT;
						continue;
					}
					// Splat's handled, we have default value
					compile_main_section(ctx, ptr->first_child->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);
					n_params_optional++;
					continue;
				}
				// Optional parameters can not be followed by required parameters
				assert(n_params_optional == 0);
				n_params_required++;
			}

			*(JUMP_OFFSET *)&(*buf)[func_jump] = (end_of_func_idx - func_jump - sizeof(JUMP_OFFSET));
			OPCODE(*buf, OP_MAKE_CLOSURE);
			DATA_JUMP_OFFSET(*buf, -(*idx - func_jump + 3*sizeof(LOCAL_VAR_INDEX) + sizeof(UPVAR_INDEX) + sizeof(int)));
			DATA_N_LOCAL_VARS(*buf, n_params_required);
			DATA_N_LOCAL_VARS(*buf, n_params_optional);
			DATA_N_LOCAL_VARS(*buf, n_locals);
			DATA_N_UPVAR_INDEX(*buf, n_uplevels);
			DATA_INT32(*buf, params_flags);

			// Doc
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_SET_CLOSURE_DOC);

			// Name
			if(node->first_child->next_sibling->next_sibling->next_sibling) {
				// Function has a name
				compile_identifier(ctx, buf, idx, node->first_child->next_sibling->next_sibling->next_sibling->name, OP_DEF_LOCAL_FUNC, OP_DEF_UPVAR_FUNC, OP_DEF_GLOBAL_FUNC);
				OPCODE(*buf, OP_SET_CLOSURE_NAME);
				L8_STR(*buf, node->first_child->next_sibling->next_sibling->next_sibling->name);
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case STR_COMPS_NODE: {
			int have_splat = 0;
			for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
				if(ptr->type == STR_COMP_SPLAT_EXPANSION_NODE) {
					have_splat = 1;
					break;
				}
			}
			if(have_splat) {
				OPCODE(*buf, OP_PUSH_NULL);
				for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
					compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
					switch(ptr->type) {
						case STR_COMP_IMM_NODE:             OPCODE(*buf, OP_MAKE_STR_IMM); break;
						case STR_COMP_EXPANSION_NODE:       OPCODE(*buf, OP_MAKE_STR_EXP); break;
						case STR_COMP_SPLAT_EXPANSION_NODE: OPCODE(*buf, OP_MAKE_STR_SPLAT_EXP); break;
					}
				}
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, argc);
				OPCODE(*buf, OP_MAKE_ARR);

				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 1);
				// OPCODE(*buf, OP_MAKE_SPLAT_STR);
				compile_identifier(ctx, buf, idx, "\"$*\"", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
				OPCODE(*buf, OP_CALL);
			} else {
				for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
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
						OPCODE(*buf, OP_PUSH_INT32);
						DATA_INT32(*buf, argc);
						OPCODE(*buf, OP_MAKE_STR);
				}
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		}
		case STR_COMP_IMM_NODE: {
			size_t l = strlen(node->name);
			if(l < 256) {
				OPCODE(*buf, OP_PUSH_L8_STR);
				L8_STR(*buf, node->name);
			} else {
				OPCODE(*buf, OP_PUSH_L32_STR);
				DATA_UINT32(*buf, l);
				ensure_room(buf, *idx, allocated, l);
				memcpy((*buf)+(*idx), node->name, l);
				(*idx) += l;
			}
			break;
		}
		case STR_COMP_EXPANSION_NODE:
		case STR_COMP_SPLAT_EXPANSION_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, need_result); break;
		case NULL_NODE:  if(need_result) { OPCODE(*buf, OP_PUSH_NULL); } break;
		case TRUE_NODE:  if(need_result) { OPCODE(*buf, OP_PUSH_TRUE); } break;
		case FALSE_NODE: if(need_result) { OPCODE(*buf, OP_PUSH_FALSE); } break;
		case DEFINED_NODE:
			compile_identifier(ctx, buf, idx, node->first_child->name, OP_LOCAL_DEF_P, OP_UPVAR_DEF_P, OP_GLOBAL_DEF_P);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case IF_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_TO_BOOL);
			OPCODE(*buf, OP_JMP_FALSE);
			if_jump = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, need_result);
			OPCODE(*buf, OP_JMP);
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			*(JUMP_OFFSET *)&(*buf)[if_jump] = *idx - if_jump - sizeof(JUMP_OFFSET); // Jump is OP_JMP_FALSE JUMP_OFFSET shorter
			if_jump = *idx - sizeof(JUMP_OFFSET);
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, need_result);
			*(JUMP_OFFSET *)&(*buf)[if_jump] = *idx - if_jump - sizeof(JUMP_OFFSET);
			break;
		case WHILE_NODE:
			loop_beg_idx = *idx;
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_TO_BOOL);
			OPCODE(*buf, OP_JMP_FALSE);
			while_jump = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			SETUP_ADDRESS_FILLING();
			continue_target_idx = loop_beg_idx; // For HANDLE_ADDRESS_FILLING
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, DONT_NEED_RESULT);
			OPCODE(*buf, OP_JMP);
			DATA_JUMP_OFFSET(*buf, -(*idx - loop_beg_idx + sizeof(JUMP_OFFSET)));
			*(JUMP_OFFSET *)&(*buf)[while_jump] = *idx - while_jump - sizeof(JUMP_OFFSET);
			HANDLE_ADDRESS_FILLING();
			if(need_result) { OPCODE(*buf, OP_PUSH_NULL); }
			break;
		case LOCAL_NODE:
		case UPVAR_NODE:
		case GLOBAL_NODE:
			for(ptr=node->first_child; ptr; ptr=ptr->next_sibling) {
				if(ptr->type != IDENTIFIER_NODE) {
					compile_main_section(ctx, ptr, buf, idx, allocated, DONT_NEED_RESULT);
				}
			}
			if(need_result) { OPCODE(*buf, OP_PUSH_NULL); }
			break;
		case HASH_LIT_NODE:
			DEBUG_COMPILER("COMPILER: %s %zu\n", "HASH NODE", *idx);
			for(ptr=node->first_child, have_hash_splat=0; ptr; ptr=ptr->next_sibling) {
				if(ptr->type == HASH_SPLAT_NODE) {
					have_hash_splat = 1;
					break;
				}
			}
			if(have_hash_splat) {
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
				OPCODE(*buf, OP_MAKE_HASH);
				for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
					if(ptr->type == HASH_SPLAT_NODE) {
						compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
						OPCODE(*buf, OP_TO_HASH);
						OPCODE(*buf, OP_HASH_UPDATE);
					} else {
						compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
						compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
						OPCODE(*buf, OP_HASH_SET);
					}
				}
			} else {
				for(argc=0, ptr=node->first_child; ptr; argc++, ptr=ptr->next_sibling) {
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
				}
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, argc);
				OPCODE(*buf, OP_MAKE_HASH);
			}
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case RETURN_NODE:
			for(i=0; i<STACK_DEPTH; i++) {
				OPCODE(*buf, OP_POP);
			}
			if(node->first_child) {
				compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			} else {
				OPCODE(*buf, OP_PUSH_NULL);
			}
			OPCODE(*buf, OP_RET);
			break;
		case AND_NODE:
		case OR_NODE:
			// TODO: optimize more for DONT_NEED_RESULT case
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_DUP);
			OPCODE(*buf, OP_TO_BOOL);
			if_jump = *idx;
			OPCODE(*buf, node->type == AND_NODE ? OP_JMP_FALSE : OP_JMP_TRUE);
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			OPCODE(*buf, OP_POP);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET); // Jump is OP_JMP_FALSE JUMP_OFFSET shorter
			if_jump = *idx - 1 - sizeof(JUMP_OFFSET);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;
		case TAND_NODE:
			if_jump = *idx;
			OPCODE(*buf, OP_TRY_START);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Set handler code location
			compile_main_section(ctx, node->first_child, buf, idx, allocated, DONT_NEED_RESULT);
			end_of_func_idx = *idx;
			OPCODE(*buf, OP_TRY_END);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Jump over handler code
			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter
			// Exception handler - start
			OPCODE(*buf, OP_THROW); // Rethrow
			// Exception handler - end
			*(JUMP_OFFSET *)&(*buf)[end_of_func_idx+1] = *idx - end_of_func_idx - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, need_result);
			break;
		case TOR_NODE:
			if_jump = *idx;
			OPCODE(*buf, OP_TRY_START);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Set handler code location

			compile_main_section(ctx, node->first_child, buf, idx, allocated, need_result);

			end_of_func_idx = *idx;
			OPCODE(*buf, OP_TRY_END);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Jump over handler code
			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter
			// Exception handler - start
			OPCODE(*buf, OP_POP); // Ignore the exception value
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, need_result);
			// Exception handler - end
			*(JUMP_OFFSET *)&(*buf)[end_of_func_idx+1] = *idx - end_of_func_idx - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter
			break;
		case GUARD_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_TO_BOOL);
			OPCODE(*buf, OP_GUARD);
			break;

		case TRY_CATCH_NODE:

			if_jump = *idx;
			OPCODE(*buf, OP_TRY_START);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Set handler code location

			compile_main_section(ctx, node->first_child, buf, idx, allocated, need_result);

			end_of_func_idx = *idx;
			OPCODE(*buf, OP_TRY_END);
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf); // Jump over handler code

			*(JUMP_OFFSET *)&(*buf)[if_jump+1] = *idx - if_jump - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter

			if(node->first_child->next_sibling) {
				// Room for return value
				OPCODE(*buf, OP_PUSH_NULL);
				OPCODE(*buf, OP_XCHG);

				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 1); // One argument for the call of handler function(s)
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0); // Make array with zero elements
				OPCODE(*buf, OP_MAKE_ARR);
				for(ptr=node->first_child->next_sibling; ptr; ptr=ptr->next_sibling) {
					compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_ARR_APPEND);
				}
				OPCODE(*buf, OP_ARR_REVERSE);
				OPCODE(*buf, OP_CALL_EXC);
				POP_IF_DONT_NEED_RESULT(*buf);
			} else {
				// No handlers, return null
				OPCODE(*buf, OP_POP); // Ignore the exception value
				if(need_result) {
					OPCODE(*buf, OP_PUSH_NULL);
				}
			}
			*(JUMP_OFFSET *)&(*buf)[end_of_func_idx+1] = *idx - end_of_func_idx - 1 - sizeof(JUMP_OFFSET); // Jump is OP_TRY_START JUMP_OFFSET shorter
			break;

		case THROW_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_THROW);
			break;

		case COMMANDS_PIPELINE_NODE:

			OPCODE(*buf, OP_PUSH_NULL); // Placeholder for return value
			// commands
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			// pipes
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);
			// options
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_MAKE_CMDS_PIPELINE);

			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 1);
			compile_identifier(ctx, buf, idx, node->first_child->name, OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
			OPCODE(*buf, OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);

			break;

		case COMMANDS_PIPE_NODE:

			// name
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			// options
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);

			OPCODE(*buf, OP_MAKE_CMDS_PIPE);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;

		case COMMAND_NODE:
			// argv
			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0); // Make array with zero elements
			OPCODE(*buf, OP_MAKE_ARR);
			for(ptr=node->first_child->next_sibling->first_child; ptr; ptr=ptr->next_sibling) {
				if(ptr->type == ARR_SPLAT_NODE) {
					compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
					OPCODE(*buf, OP_TO_ARR);
					OPCODE(*buf, OP_ARR_CONCAT);
					continue;
				}
				compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
				OPCODE(*buf, OP_ARR_APPEND);
			}
			// redirects
			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0); // Make array with zero elements
			OPCODE(*buf, OP_MAKE_ARR);
			for(ptr=node->first_child->next_sibling->next_sibling->first_child; ptr; ptr=ptr->next_sibling) {
				compile_main_section(ctx, ptr, buf, idx, allocated, NEED_RESULT);
				OPCODE(*buf, OP_ARR_APPEND);
			}
			// options
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);

			OPCODE(*buf, OP_MAKE_CMD);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;

		case BREAK_NODE:
			assert(ctx->fill_in_break_addrs_ptr < COMPILE_MAX_FILL_IN_LEN);
			OPCODE(*buf, OP_JMP);
			ctx->fill_in_break_addrs[ctx->fill_in_break_addrs_ptr++] = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			break;

		case CONTINUE_NODE:
			assert(ctx->fill_in_continue_addrs_ptr < COMPILE_MAX_FILL_IN_LEN);
			OPCODE(*buf, OP_JMP);
			ctx->fill_in_continue_addrs[ctx->fill_in_continue_addrs_ptr++] = *idx;
			DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			break;

		case REDIR_NODE:
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			compile_main_section(ctx, node->first_child->next_sibling->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_MAKE_REDIR);
			break;

		case SWITCH_NODE:
			// XXX: Fix and test STACK_DEPTH
			// XXX: Check for/while { ... case { ... break } ... } situation because break addresses are used in switch too.
			// TODO: assert jump ranges
			IF_NOT_SWITCH_COND {
				compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
				STACK_DEPTH++;
			}
			SETUP_ADDRESS_FILLING();
			continue_target_idx = 0; // Should not appear there!
			// TODO: make sure that leaving the section pops the switch value from the stack
			cond_jump = 0;
			for(ptr=node->first_child->next_sibling; ptr; ptr=ptr->next_sibling) {
				if(cond_jump) {
					// Jump to next comparison
					*(JUMP_OFFSET *)&(*buf)[cond_jump] = *idx - (cond_jump + sizeof(JUMP_OFFSET));
					cond_jump = 0;
				}
				IF_NOT_SWITCH_COND {
					OPCODE(*buf, OP_DUP);
					OPCODE(*buf, OP_PUSH_NULL); // Result placeholder
					OPCODE(*buf, OP_XCHG);
				}
				// The value to compare to
				compile_main_section(ctx, ptr->first_child, buf, idx, allocated, NEED_RESULT);
				switch((switch_node_subtype)node->number) {
					case SWITCH_NODE_SWITCH:
					case SWITCH_NODE_ESWITCH:
						OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 2);
						compile_identifier(ctx, buf, idx, "==", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
						OPCODE(*buf, OP_CALL);
						break;
					case SWITCH_NODE_MATCH:
					case SWITCH_NODE_EMATCH:
						OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 2);
						compile_identifier(ctx, buf, idx, "match", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
						OPCODE(*buf, OP_CALL);
					case SWITCH_NODE_COND:
					case SWITCH_NODE_ECOND:
						OPCODE(*buf, OP_TO_BOOL);
						break;
					default:
						fprintf(stderr, "ERROR: SWITCH_NODE subtype %li %i\n", node->number, SWITCH_NODE_COND);
						assert(0 == "Unsupported SWITCH_NODE subtype");
				}
				OPCODE(*buf, OP_JMP_FALSE);
				cond_jump = *idx;
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
				// Code block to execute when values match
				compile_main_section(ctx, ptr->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
				IF_NOT_SWITCH_COND {
					// Get rid of original value, preserving the match result
					OPCODE(*buf, OP_XCHG);
					OPCODE(*buf, OP_POP);
				}
				// Break
				assert(ctx->fill_in_break_addrs_ptr < COMPILE_MAX_FILL_IN_LEN);
				OPCODE(*buf, OP_JMP);
				ctx->fill_in_break_addrs[ctx->fill_in_break_addrs_ptr++] = *idx;
				DATA_JUMP_OFFSET_PLACEHOLDER(*buf);
			}
			if(cond_jump) {
				// Jump to next comparison
				*(JUMP_OFFSET *)&(*buf)[cond_jump] = *idx - (cond_jump + sizeof(JUMP_OFFSET));
			}
			// TOOD: optimize - OP_PUSH_NULL is not needed if result is not needed
			if(node->number & 1) {
				OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 0);
				compile_identifier(ctx, buf, idx, "SwitchFail", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
				// TODO: attribute with offending value
				OPCODE(*buf, OP_CALL);
				OPCODE(*buf, OP_THROW);
			} else {
				IF_NOT_SWITCH_COND {
					OPCODE(*buf, OP_POP); // Get rid of original value
				}
				OPCODE(*buf, OP_PUSH_NULL);
			}
			HANDLE_ADDRESS_FILLING();
			POP_IF_DONT_NEED_RESULT(*buf);
			IF_NOT_SWITCH_COND {
				STACK_DEPTH--;
			}
			break;

		case SUPER_NODE:
			if(need_result) {
				OPCODE(*buf, OP_SUPER);
				STACK_DEPTH++;
			}
			break;

		case REGEXP_NODE:
			OPCODE(*buf, OP_PUSH_NULL);
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			compile_main_section(ctx, node->first_child->next_sibling, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 2);
			compile_identifier(ctx, buf, idx, "//", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
			OPCODE(*buf, OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;

		case TABLE_LIT_NODE:
			OPCODE(*buf, OP_PUSH_NULL);
			compile_main_section(ctx, node->first_child, buf, idx, allocated, NEED_RESULT);
			OPCODE(*buf, OP_PUSH_INT32); DATA_INT32(*buf, 1);
			compile_identifier(ctx, buf, idx, "table", OP_FETCH_LOCAL, OP_FETCH_UPVAR, OP_FETCH_GLOBAL);
			OPCODE(*buf, OP_CALL);
			POP_IF_DONT_NEED_RESULT(*buf);
			break;


		default:
			fprintf(stderr, "Node type %i\n", node->type);
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

//		section type 2 data: globals patching
//			uint16 - number of items
//			item:
//				Lstr
//				uint16 - number of locations
//				uint32[] - locations

size_t calculate_init_section_size(COMPILATION_CONTEXT *ctx) {
	size_t ret = sizeof(BYTECODE_GLOBALS_COUNT);
	SYMBOL_TABLE *globals;
	DEBUG_COMPILER("%s", "entering calculate_init_section_size()\n");
	FOR_GLOBALS(
			// Lstr
			// uint16 - number of locations
			// uint32[] - locations
			ret += 1 + strlen(globals->name) + sizeof(BYTECODE_GLOBALS_LOC_COUNT) + utarray_len(globals->offsets) * sizeof(BYTECODE_GLOBALS_OFFSET);
	)

	DEBUG_COMPILER("leaving calculate_init_section_size() -> %zu\n", ret);
	return ret;
}

// TODO: DATA_UINT.. -> DATA_GLOBALS_...
void compile_init_section(COMPILATION_CONTEXT *ctx, char **init_buf, size_t *idx) {
	// TODO: check that all offsets are really in UINT16 range.
	char *buf = NULL;
	SYMBOL_TABLE *globals;
	size_t init_section_size;
	size_t i = 0;
	DEBUG_COMPILER("%s", "entering compile_init_section()\n");
	init_section_size = calculate_init_section_size(ctx);
	buf = NGS_MALLOC(init_section_size);
	FOR_GLOBALS(
		i++;
	)
	// printf("number of globals to patch: %zu, section size %zu\n", i, init_section_size);
	DATA_UINT16(buf, i);
	FOR_GLOBALS(
			// Lstr
			// uint16 - number of locations
			// uint32[] - locations
			size_t len;
			L8_STR(buf, globals->name);
			len = utarray_len(globals->offsets);
			DATA_UINT16(buf, len);
			for(i = 0; i < len; i++) {
				// printf("compile_init_section() global i=%zu name=%s offset=%d idx=%zu\n", i, globals->name, *(int *)utarray_eltptr(globals->offsets, i), *idx);
				DATA_UINT32(buf, *(uint32_t *)utarray_eltptr(globals->offsets, i))
			}
	);
	assert(*idx == init_section_size); // the init section size calculations must be correct
	DEBUG_COMPILER("%s", "leaving compile_init_section()\n");
	*init_buf = buf;
}

void compile_source_location_section(COMPILATION_CONTEXT *ctx, char **buf, size_t *idx) {
	*buf = NGS_MALLOC_ATOMIC(2 + 1 + strlen(ctx->source_file_name) + 4 + ctx->source_tracking_entries_count * sizeof(source_tracking_entry));
	*idx = 0;
	// Number of files' names. Currently just one is supported
	// More than one file could be result of compiling something to NGS language
	DATA_UINT16(*buf, 1);
	L8_STR(*buf, ctx->source_file_name);
	DATA_UINT32(*buf, ctx->source_tracking_entries_count);
	memcpy(*buf + *idx, ctx->source_tracking_entries, ctx->source_tracking_entries_count * sizeof(source_tracking_entry));
	*idx += ctx->source_tracking_entries_count * sizeof(source_tracking_entry);
}

// ast_node - the top level node
char *compile(ast_node *node, char *source_file_name, size_t *len) {

	char *buf = NGS_MALLOC(COMPILE_INITIAL_BUF_SIZE);
	size_t main_allocated = COMPILE_INITIAL_BUF_SIZE;
	size_t l;
	COMPILATION_CONTEXT ctx;
	BYTECODE_HANDLE *bytecode;

	vm_init(&(ctx.vm), 0, NULL);
	ctx.globals = NULL;
	// TODO: assert that got memory?
	ctx.locals_ptr = 0;
	ctx.stack_depth[0] = 0;
	ctx.source_file_name = source_file_name;
	ctx.source_tracking_entries_count = 0;
	ctx.source_tracking_entries_allocated = 1024;
	ctx.source_tracking_entries = NGS_MALLOC(sizeof(source_tracking_entry) * ctx.source_tracking_entries_allocated);
	ctx.fill_in_break_addrs_ptr = 0;
	ctx.fill_in_continue_addrs_ptr = 0;

	bytecode = ngs_create_bytecode();

	// CODE SECTION
	l = 0;
	compile_main_section(&ctx, node, &buf, &l, &main_allocated, NEED_RESULT);
	ensure_room(&buf, l, &main_allocated, 1);
	buf[l++] = OP_RET;
	ngs_add_bytecode_section(bytecode, BYTECODE_SECTION_TYPE_CODE, l, buf);

	// GLOBALS SECTION
	l = 0;
	compile_init_section(&ctx, &buf, &l);
	ngs_add_bytecode_section(bytecode, BYTECODE_SECTION_TYPE_GLOBALS, l, buf);

	// SOURCE LOCATION TRACKING SECTION
	l = 0;
	compile_source_location_section(&ctx, &buf, &l);
	ngs_add_bytecode_section(bytecode, BYTECODE_SECTION_TYPE_SRCLOC, l, buf);

	*len = bytecode->len;

	return bytecode->data;
}
#undef LOCALS
