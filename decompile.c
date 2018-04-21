#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include "ngs.h"
#include "vm.h"

// TODO: get rid of int16_t and int32_t, use types such as JUMP_OFFSET and PATCH_OFFSET instead.

void decompile(const char *buf, const size_t start, const size_t end) {
	size_t idx, orig_idx;
	unsigned char opcode;
	char info_buf[1024];
	uint32_t str_len;

	for(idx=start; idx < end; ) {
		orig_idx = idx;
		opcode = buf[idx++];
		info_buf[0] = 0;
		switch(opcode) {
			case OP_JMP:
			case OP_JMP_TRUE:
			case OP_JMP_FALSE:
			case OP_TRY_START:
			case OP_TRY_END:
				sprintf(info_buf, " %+05d (%04zu)", *(JUMP_OFFSET *)&buf[idx], idx + *(JUMP_OFFSET *)&buf[idx] + sizeof(JUMP_OFFSET));
				idx+=sizeof(JUMP_OFFSET);
				break;
			case OP_MAKE_CLOSURE:
				sprintf(info_buf, " code=%+05d (%04zu), n_params_required=%d, n_params_optional=%d, n_locals=%d, n_uplevels=%d, params_flags=%d",
						*(JUMP_OFFSET *)&buf[idx],
						idx + *(JUMP_OFFSET *)&buf[idx] + sizeof(JUMP_OFFSET) + 3*sizeof(LOCAL_VAR_INDEX) + sizeof(UPVAR_INDEX),
						*(LOCAL_VAR_INDEX *)&buf[idx+sizeof(JUMP_OFFSET)],
						*(LOCAL_VAR_INDEX *)&buf[idx+sizeof(JUMP_OFFSET)+1*sizeof(LOCAL_VAR_INDEX)],
						*(LOCAL_VAR_INDEX *)&buf[idx+sizeof(JUMP_OFFSET)+2*sizeof(LOCAL_VAR_INDEX)],
						*(UPVAR_INDEX *)&buf[idx+sizeof(JUMP_OFFSET)+3*sizeof(LOCAL_VAR_INDEX)],
						*(int *)&buf[idx+sizeof(JUMP_OFFSET)+3*sizeof(LOCAL_VAR_INDEX)+sizeof(UPVAR_INDEX)]
				);
				idx+=sizeof(JUMP_OFFSET) + 3*sizeof(LOCAL_VAR_INDEX) + sizeof(UPVAR_INDEX) + sizeof(int);
				break;
			case OP_PATCH:
				sprintf(info_buf, " %+05d (%04zu)", *(PATCH_OFFSET *)&buf[idx], idx + *(PATCH_OFFSET *)&buf[idx] + sizeof(PATCH_OFFSET));
				idx+=sizeof(PATCH_OFFSET);
				break;
			case OP_FETCH_GLOBAL:
			case OP_STORE_GLOBAL:
			case OP_GLOBAL_DEF_P:
			case OP_DEF_GLOBAL_FUNC:
				sprintf(info_buf, " %d", *(int16_t *)&buf[idx]); idx+=2; break;
			case OP_FETCH_LOCAL:
			case OP_STORE_LOCAL:
			case OP_LOCAL_DEF_P:
			case OP_DEF_LOCAL_FUNC:
				sprintf(info_buf, " %d", *(LOCAL_VAR_INDEX *)&buf[idx]); idx+=sizeof(LOCAL_VAR_INDEX); break;
			case OP_FETCH_UPVAR:
			case OP_STORE_UPVAR:
			case OP_UPVAR_DEF_P:
			case OP_DEF_UPVAR_FUNC:
				sprintf(info_buf, " uplevel=%d, index=%d", *(UPVAR_INDEX *)&buf[idx], *(LOCAL_VAR_INDEX *)&buf[idx+sizeof(UPVAR_INDEX)]); idx+=sizeof(UPVAR_INDEX)+sizeof(LOCAL_VAR_INDEX); break;
			case OP_PUSH_INT32:
				sprintf(info_buf, " %" PRIi32, *(int32_t *)&buf[idx]); idx+=4; break;
			case OP_PUSH_INT64:
				sprintf(info_buf, " %" PRIi64, *(int64_t *)&buf[idx]); idx+=8; break;
			case OP_PUSH_REAL:
				sprintf(info_buf, " " NGS_REAL_FMT, *(NGS_REAL *)&buf[idx]); idx+=sizeof(NGS_REAL); break;
			case OP_PUSH_L8_STR:
				str_len = (unsigned char)buf[idx++];
				sprintf(info_buf, " %.*s", str_len, &buf[idx]);
				idx += str_len;
				break;
			case OP_PUSH_L32_STR:
				str_len = *(uint32_t *) &buf[idx++];
				sprintf(info_buf, " (len %"PRIu32", not showing)", str_len);
				idx += str_len;
				break;
		}
		if(opcode <= sizeof(opcodes_names) / sizeof(char *)) {
			printf("DECOMPILE [%04zu] %s%s\n", orig_idx, opcodes_names[opcode], info_buf);
		}
	}
}
