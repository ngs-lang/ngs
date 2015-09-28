#ifndef COMPILE_H
#define COMPILE_H

#define SYMBOL_OFFSETS_INITIAL_SIZE (16)
#define COMPILE_INITIAL_BUF_SIZE (16384)

#include <utarray.h>

static const UT_icd ut_size_t_icd _UNUSED_ = {sizeof(size_t),NULL,NULL,NULL};

typedef struct symbol_table {
	char *name;
	UT_hash_handle hh;
	UT_array *offsets;
	int is_predefinded_global;
	size_t index;
} SYMBOL_TABLE;

typedef struct compilation_context {
	VM vm;
	int need_result;
	SYMBOL_TABLE *globals;
} COMPILATION_CONTEXT;

#endif
