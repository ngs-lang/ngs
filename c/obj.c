#include <execinfo.h>
#include <stdio.h>
#include "obj.h"

void _dump(VALUE v, int level) {
	char **symbols;
	void *symbols_buffer[1];
	if(IS_NULL(v)) { printf("%*s* null\n", level << 1, ""); }
	if(IS_TRUE(v)) { printf("%*s* true\n", level << 1, ""); }
	if(IS_FALSE(v)) { printf("%*s* false\n", level << 1, ""); }
	if(IS_UNDEF(v)) { printf("%*s* undef\n", level << 1, ""); }
	if(IS_INT(v)) {
		printf("%*s* int %ld\n", level << 1, "", GET_INT(v));
		goto _dump_exit;
	}
	if(IS_STRING(v)) {
		printf("%*s* string(len=%d) %.*s\n", level << 1, "", OBJ_LEN(v), OBJ_LEN(v), OBJ_DATA_PTR(v));
		goto _dump_exit;
	}
	if(IS_NATIVE_METHOD(v)) {
		symbols_buffer[0] = OBJ_DATA_PTR(v);
		symbols = backtrace_symbols(symbols_buffer, 1);
		printf("%*s* native method %s at 0x%zX\n", level << 1, "", symbols[0], OBJ_DATA_PTR(v));
		goto _dump_exit;
	}


_dump_exit:
	return;
}

void dump(VALUE v) {
	_dump(v, 0);
}

void dump_titled(char *title, VALUE v) {
	printf("=== [ dump %s ] ===\n", title);
	dump(v);
}
