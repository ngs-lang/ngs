#ifndef VM_H
#define VM_H
#include <utarray.h>
#include <uthash.h>
#include "ngs.h"
#include "obj.h"

#define NGS_UNUSED __attribute__((unused))

// --- BYTECODE ---------------------------------

//	* bytecode format
//
//		"NGS BYTECODE" (String of length 12, not zero-terminated)
//
//		0x0102030405060708 - 8 bytes for byte ordering check
//
//		uint16 - number of sections
//
//		section:
//			uint16 - type
//			uint32 - data len
//			data
//
//		section type 1 data: code
//		section type 2 data: globals patching
//			uint16 - number of items
//			item:
//				Lstr
//				uint16 - number of locations
//				uint32[] - locations

typedef uint64_t BYTECODE_ORDER_CHECK;
typedef uint16_t BYTECODE_SECTIONS_COUNT;
typedef uint16_t BYTECODE_SECTION_TYPE;
typedef uint32_t BYTECODE_SECTION_LEN;

typedef uint16_t BYTECODE_GLOBALS_COUNT;
typedef uint16_t BYTECODE_GLOBALS_LOC_COUNT;
typedef uint32_t BYTECODE_GLOBALS_OFFSET;

extern char BYTECODE_SIGNATURE[];

#define BYTECODE_SECTION_TYPE_CODE    (1)
#define BYTECODE_SECTION_TYPE_GLOBALS (2)
#define BYTECODE_ORDER_CHECK_VAL      (0x0102030405060708)

typedef struct {
	char *data;
	size_t len;
	BYTECODE_SECTIONS_COUNT sections_count, next_section_num;
	char *next_section_ptr;
} BYTECODE_HANDLE;


// --- VM ---------------------------------------

#define MAX_STACK          (1024)
#define MAX_FRAMES           (64)
#define MAX_TRIES_PER_FRAME   (8)

typedef uint16_t PATCH_OFFSET;
typedef int16_t JUMP_OFFSET;

typedef size_t IP;

typedef struct var_index {
	char *name;
	GLOBAL_VAR_INDEX index;
	UT_hash_handle hh;
} VAR_INDEX;

// WIP
typedef struct interned_string {
	char *str;
	UT_hash_handle hh;
} INTERNED_STRING;

typedef struct {
	IP catch_ip;
	size_t saved_stack_ptr;
} TRY_INFO;

typedef struct {
	// TODO: smarter allocation of locals when can't be captured by clousre,
	//       probably on stack.
	VALUE *locals;
	VALUE closure;

	TRY_INFO try_info[MAX_TRIES_PER_FRAME];
	int try_info_ptr;

	// Enable/disable using impl_not_found_hook
	int do_call_impl_not_found_hook;

	// For stack trace
	IP last_ip;

} FRAME;

// Plan: have exactly one context per thread.
typedef struct context {
	VALUE stack[MAX_STACK];
	size_t stack_ptr;

	FRAME frames[MAX_FRAMES];
	size_t frame_ptr;
} CTX;

typedef struct vm_struct {
	char *bytecode;
	size_t bytecode_len;
	VALUE *globals;
	size_t globals_len;
	VAR_INDEX *globals_indexes;
	char **globals_names;

	NGS_TYPE *Null;
	NGS_TYPE *Bool;
	NGS_TYPE *Int;
	NGS_TYPE *Str;
	NGS_TYPE *Arr;
	NGS_TYPE *Fun;
	NGS_TYPE *Any;
		NGS_TYPE *BasicTypeInstance;
		NGS_TYPE *NormalTypeInstance;
	NGS_TYPE *Seq;
	NGS_TYPE *Type;
		NGS_TYPE *BasicType;
		NGS_TYPE *NormalType;
	NGS_TYPE *Hash;
	NGS_TYPE *CLib;
	NGS_TYPE *CSym;

	VALUE Exception;
		VALUE Error;
			VALUE LookupFail;
				VALUE KeyNotFound;
				VALUE IndexNotFound;
				VALUE AttrNotFound;
				VALUE GlobalNotFound;
			VALUE InvalidArgument;
			VALUE CompileFail;
			VALUE CallFail;
				VALUE DontKnowHowToCall;
				VALUE ImplNotFound;

	VALUE Backtrace;

	VALUE Command;
	VALUE Range;
		VALUE InclusiveRange;
		VALUE ExclusiveRange;

	VALUE impl_not_found_hook;
	VALUE init;

} VM;

enum opcodes {
	OP_HALT,
	OP_PUSH_NULL,
	OP_PUSH_FALSE,
	OP_PUSH_TRUE,
	OP_PUSH_INT,
	OP_PUSH_L_STR,
	OP_DUP,
	OP_POP,
	OP_XCHG,
	OP_RESOLVE_GLOBAL,
	OP_PATCH,
	OP_FETCH_GLOBAL,
	OP_STORE_GLOBAL,
	OP_FETCH_LOCAL,
	OP_STORE_LOCAL,
	OP_CALL,
	OP_CALL_EXC,
	OP_CALL_ARR,
	OP_RET,
	OP_JMP,
	OP_JMP_TRUE,
	OP_JMP_FALSE,
	OP_MAKE_ARR,
	OP_MAKE_CLOSURE,
	OP_TO_STR,
	OP_MAKE_STR,
	OP_PUSH_EMPTY_STR,
	OP_GLOBAL_DEF_P,
	OP_LOCAL_DEF_P,
	OP_DEF_GLOBAL_FUNC,
	OP_DEF_LOCAL_FUNC,
	OP_FETCH_UPVAR,
	OP_STORE_UPVAR,
	OP_UPVAR_DEF_P,
	OP_DEF_UPVAR_FUNC,
	OP_MAKE_HASH,
	OP_TO_BOOL,
	OP_TO_ARR,
	OP_ARR_APPEND,
	OP_ARR_CONCAT,
	OP_GUARD,
	OP_TRY_START,
	OP_TRY_END,
	OP_ARR_REVERSE,
	OP_THROW,
	OP_MAKE_CMD,
	NUMBER_OF_OPCODES,
};

extern char *opcodes_names[NUMBER_OF_OPCODES];

typedef METHOD_RESULT (*VM_FUNC)(const VALUE *argv, VALUE *result);
typedef METHOD_RESULT (*VM_EXT_FUNC)(VM *vm, CTX *ctx, const VALUE *argv, VALUE *result);
void vm_init(VM *vm, int argc, char **argv);
size_t vm_load_bytecode(VM *vm, char *bc);
void ctx_init(CTX *ctx);
GLOBAL_VAR_INDEX check_global_index(VM *vm, const char *name, size_t name_len, int *found);
GLOBAL_VAR_INDEX get_global_index(VM *vm, const char *name, size_t name_len);

static const UT_icd ut_value_icd _UNUSED_ = {sizeof(VALUE),NULL,NULL,NULL};
// typedef int VM_INT;
void set_global(VM *vm, const char *name, VALUE v);
METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result);
METHOD_RESULT vm_call(VM *vm, CTX *ctx, VALUE *result, const VALUE callable, const LOCAL_VAR_INDEX argc, const VALUE *argv);
BYTECODE_HANDLE *ngs_create_bytecode();
void ngs_add_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE type, BYTECODE_SECTION_LEN len, char *data);
BYTECODE_HANDLE *ngs_start_unserializing_bytecode(char *data);
void ngs_fetch_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE *type, BYTECODE_SECTION_LEN *len, char **data);
// In obj.c
VALUE make_backtrace(VM *vm, CTX *ctx);
#endif
