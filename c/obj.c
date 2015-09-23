#include <stdio.h>
#include "obj.h"

void _dump(VALUE v, int level) {
	if(IS_INT(v)) {
		printf("%*s* int %ld\n", level << 1, "", GET_INT(v));
		goto _dump_exit;
	}
	if(IS_STRING(v)) {
		printf("%*s* string(len=%d) %.*s\n", level << 1, "", OBJ_LEN(v), OBJ_LEN(v), OBJ_DATA_PTR(v));
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
