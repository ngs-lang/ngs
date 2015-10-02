#ifndef DECOMPILE_H
#define DECOMPILE_H

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

void decompile(const char *buf, const size_t start, const size_t end) {
	size_t idx, orig_idx;
	unsigned char opcode;
	char info_buf[1024];
	unsigned char str_len;

	for(idx=start; idx < end; ) {
		orig_idx = idx;
		opcode = buf[idx++];
		info_buf[0] = 0;
		switch(opcode) {
			case OP_JMP:
			case OP_JMP_TRUE:
			case OP_JMP_FALSE:
			case OP_PATCH:
				sprintf(info_buf, " %+05d (%04zu)", *(int16_t *)&buf[idx], idx + *(int16_t *)&buf[idx] + 2);
				idx+=2;
				break;

			case OP_FETCH_GLOBAL:
			case OP_STORE_GLOBAL:
				sprintf(info_buf, " %d", *(int16_t *)&buf[idx]); idx+=2; break;
			case OP_PUSH_INT:
				sprintf(info_buf, " %d", *(int32_t *)&buf[idx]); idx+=4; break;
			case OP_PUSH_L_STR:
				str_len = (unsigned char)buf[idx++];
				sprintf(info_buf, " %.*s", str_len, &buf[idx]);
				idx += str_len;
		}
		if(opcode <= sizeof(opcodes_names) / sizeof(char *)) {
			printf("DECOMPILE [%04zu] %s%s\n", orig_idx, opcodes_names[opcode], info_buf);
		}
	}
}

#endif
