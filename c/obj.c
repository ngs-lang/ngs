#ifndef OBJ_C
#define OBJ_C
#include <execinfo.h>
#include <stdio.h>
#include <assert.h>
#include "ngs.h"
#include "obj.h"

void _dump(VALUE v, int level) {
	char **symbols;
	void *symbols_buffer[1];
	VALUE *ptr;
	size_t i;

	if(IS_NULL(v))  { printf("%*s* null\n",    level << 1, ""); goto exit; }
	if(IS_TRUE(v))  { printf("%*s* true\n",    level << 1, ""); goto exit; }
	if(IS_FALSE(v)) { printf("%*s* false\n",   level << 1, ""); goto exit; }
	if(IS_UNDEF(v)) { printf("%*s* undef\n",   level << 1, ""); goto exit; }

	if(IS_INT(v))   { printf("%*s* int %ld\n", level << 1, "", GET_INT(v)); goto exit; }

	if(IS_STRING(v)) {
		printf("%*s* string(len=%zu) %.*s\n", level << 1, "", OBJ_LEN(v), (int) OBJ_LEN(v), (char *)OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_NATIVE_METHOD(v)) {
		symbols_buffer[0] = OBJ_DATA_PTR(v);
		symbols = backtrace_symbols(symbols_buffer, 1);
		printf("%*s* native method %s at %p\n", level << 1, "", symbols[0], OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_CLOSURE(v)) {
		printf("%*s* closure ip=%zu\n", level << 1, "", CLOSURE_OBJ_IP(v));
		goto exit;
	}

	if(IS_ARRAY(v)) {
		printf("%*s* array of length %zu\n", level << 1, "", OBJ_LEN(v));
		for(i=0, ptr=(VALUE *)OBJ_DATA_PTR(v); i<OBJ_LEN(v); i++, ptr++) {
			_dump(*ptr, level+1);
		}
		goto exit;
	}


exit:
	return;
}

VALUE make_var_len_obj(const size_t item_size, const size_t len) {

	VALUE v;
	VAR_LEN_OBJECT *vlo;

	vlo = NGS_MALLOC(sizeof(*vlo));
	vlo->base.type.num = OBJ_TYPE_ARRAY;
	vlo->len = len;
	vlo->item_size = item_size;
	if(len) {
		vlo->base.val.ptr = NGS_MALLOC(item_size*len);
		assert(vlo->base.val.ptr);
	} else {
		vlo->base.val.ptr = NULL;
	}

	SET_OBJ(v, vlo);

	return v;
}

VALUE make_closure_obj(size_t ip) {

	VALUE v;
	CLOSURE_OBJECT *c;

	c = NGS_MALLOC(sizeof(*c));
	c->base.type.num = OBJ_TYPE_CLOSURE;
	c->ip = ip;

	SET_OBJ(v, c);

	return v;
}

void dump(VALUE v) {
	_dump(v, 0);
}

void dump_titled(char *title, VALUE v) {
	printf("=== [ dump %s ] ===\n", title);
	dump(v);
}
#endif
