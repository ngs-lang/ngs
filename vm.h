#ifndef VM_H
#define VM_H
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
//		section type 0x101 data: source tracking
//			* Files names
//				uint16 - N files
//				Lstring - file name
//			* IP positions
//				uint32 - N entries
//				N entries:
//					uint32 - ip
//					uint32 x 4 - location
//					uint16 - file number

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
#define BYTECODE_SECTION_TYPE_SRCLOC  (0x101)
#define BYTECODE_ORDER_CHECK_VAL      (0x0102030405060708)

typedef struct {
	char *data;
	size_t len;
	BYTECODE_SECTIONS_COUNT sections_count, next_section_num;
	char *next_section_ptr;
} BYTECODE_HANDLE;


// --- VM ---------------------------------------

#define MAX_STACK          (2048)
#define MAX_FRAMES         (1024)
#define MAX_TRIES_PER_FRAME   (8)
#define MAX_ARGS             (16)

// TODO: range check
// TODO: smaller types for shorter jumps?
typedef uint32_t PATCH_OFFSET;
typedef int32_t JUMP_OFFSET;

typedef size_t IP;

typedef struct {
	IP catch_ip;
	size_t saved_stack_ptr;
} TRY_INFO;

typedef struct {
	// TODO: smarter allocation of locals when can't be captured by clousre,
	//       probably on stack.
	VALUE *locals;
	VALUE closure;

	VALUE *arr_callable;
	int *arr_callable_idx;

	TRY_INFO try_info[MAX_TRIES_PER_FRAME];
	int try_info_ptr;

	// Enable/disable using method_not_found_handler
	int do_call_method_not_found_handler;
	int do_call_call;

	// For stack trace
	IP last_ip;

} FRAME;

// Plan: have exactly one context per thread.
typedef struct CTX {
	VALUE stack[MAX_STACK];
	size_t stack_ptr;

	FRAME frames[MAX_FRAMES];
	size_t frame_ptr;
} CTX;

typedef struct {
	uint32_t ip;
	uint32_t source_location[4]; // start line+col, end line+col
	uint16_t source_file_name_idx;
} source_tracking_entry;

typedef struct {
	size_t start_ip, len;

	char **files_names;
	size_t files_names_allocated, files_names_len;

	size_t source_tracking_entries_count;
	source_tracking_entry *source_tracking_entries;

} VM_REGION;

struct VM {
	char *bytecode;
	size_t bytecode_len;
	VALUE *globals;
	size_t globals_len;
	VALUE globals_indexes;
	char **globals_names;

	VM_REGION *regions;
	size_t regions_len, regions_allocated;

	VALUE last_doc_hash;

	VALUE *type_by_value_tag[MAX_VALUE_TAG_VALUE+1];
	VALUE *type_by_t_obj_type_id[MAX_T_OBJ_TYPE_ID+1]; // XXX

	VALUE Null;
	VALUE Bool;
	VALUE Int;
	VALUE Real;
	VALUE Str;
	VALUE Arr;
	VALUE Fun;
		VALUE UserDefinedMethod;
		VALUE NativeMethod;
		VALUE MultiMethod;
	VALUE Any;
		VALUE BasicTypeInstance;
		VALUE NormalTypeInstance;
	VALUE Seq;
	VALUE Type;
		VALUE BasicType;
		VALUE NormalType;
	VALUE Hash;
		VALUE Namespace;
	VALUE LLHashEntry;
	VALUE CLib;
	VALUE CSym;
	VALUE RegExp;

	VALUE NormalTypeConstructor;

	// Not sure about naming convention
	VALUE c_pthread_t;
	VALUE c_pthread_attr_t;
	VALUE c_pthread_mutex_t;
	VALUE c_pthread_mutexattr_t;
	VALUE c_pthread_cond_t;
	// TODO: VALUE c_pthread_mutex_tAttr;

	VALUE c_ffi_type;
	VALUE c_ffi_cif;

	VALUE C_DIR;

	// *** Add new VALUE MyType above this line ***

	VALUE Exception;
		VALUE Error;
			VALUE InternalError;
			VALUE LookupFail;
				VALUE KeyNotFound;
				VALUE IndexNotFound;
					VALUE EmptyArrayFail;
				VALUE FieldNotFound;
				VALUE GlobalNotFound;
			VALUE UndefinedLocalVar;
			VALUE UndefinedUpVar;
			VALUE InvalidArgument;
				VALUE DivisionByZero;
			VALUE CompileFail;
			VALUE RegExpCompileFail;
			VALUE CallFail;
				VALUE DontKnowHowToCall;
				VALUE MethodNotFound;
				VALUE StackDepthFail;
				VALUE ArgsMismatch;
			VALUE SwitchFail;
			VALUE DlopenFail;
			VALUE DecodeFail;
				VALUE JsonDecodeFail;
			VALUE StackOverflow;

	VALUE Backtrace;

	VALUE CommandsPipeline;
	VALUE CommandsPipe;
	VALUE Command;
	VALUE CommandRedir;

	VALUE NumRange;

	VALUE Stat;
	VALUE c_tm;

	VALUE NgsStrComp;
		VALUE NgsStrCompImm;
		VALUE NgsStrCompExp;
		VALUE NgsStrCompSplatExp;

	VALUE method_not_found_handler;
	VALUE global_not_found_handler;
	VALUE init;
	VALUE call;
	VALUE pattern_match; // =~

	VALUE eqeq;

	// awk -F '[ ;]' '$1 == "FFI_EXTERN" {print "VALUE c_" $3 ";"}' /usr/include/x86_64-linux-gnu/ffi.h
	VALUE c_ffi_type_void;
	VALUE c_ffi_type_uint8;
	VALUE c_ffi_type_sint8;
	VALUE c_ffi_type_uint16;
	VALUE c_ffi_type_sint16;
	VALUE c_ffi_type_uint32;
	VALUE c_ffi_type_sint32;
	VALUE c_ffi_type_uint64;
	VALUE c_ffi_type_sint64;
	VALUE c_ffi_type_float;
	VALUE c_ffi_type_double;
	VALUE c_ffi_type_pointer;
	VALUE c_ffi_type_longdouble;
	VALUE c_ffi_type_complex_float;
	VALUE c_ffi_type_complex_double;
	VALUE c_ffi_type_complex_longdouble;

	VALUE c_ffi_type_string;

	// networking
	VALUE c_sockaddr;
	VALUE c_sockaddr_un;
	VALUE c_sockaddr_in;

};


typedef struct {
	VM *vm;
	VALUE f;
	VALUE arg;
} NGS_PTHREAD_INIT_INFO;

enum opcodes {
	OP_HALT,
	OP_PUSH_NULL,
	OP_PUSH_FALSE,
	OP_PUSH_TRUE,
	OP_PUSH_INT32,
	OP_PUSH_INT64,
	OP_PUSH_REAL,
	OP_PUSH_L8_STR,
	OP_PUSH_L32_STR,
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
	OP_MAKE_STR_IMM,
	OP_MAKE_STR_EXP,
	OP_MAKE_STR_SPLAT_EXP,
	OP_PUSH_EMPTY_STR,
	OP_DEF_GLOBAL_FUNC,
	OP_DEF_LOCAL_FUNC,
	OP_FETCH_UPVAR,
	OP_STORE_UPVAR,
	OP_DEF_UPVAR_FUNC,
	OP_MAKE_HASH,
	OP_TO_BOOL,
	OP_TO_ARR,
	OP_TO_HASH,
	OP_ARR_APPEND,
	OP_ARR_APPEND2,
	OP_ARR_CONCAT,
	OP_GUARD,
	OP_TRY_START,
	OP_TRY_END,
	OP_ARR_REVERSE,
	OP_THROW,
	OP_MAKE_CMDS_PIPELINE,
	OP_MAKE_CMDS_PIPE,
	OP_MAKE_CMD,
	OP_SET_CLOSURE_NAME,
	OP_SET_CLOSURE_DOC,
	OP_SET_CLOSURE_NS,
	OP_HASH_SET,
	OP_HASH_UPDATE,
	OP_PUSH_KWARGS_MARKER,
	OP_MAKE_REDIR,
	OP_SUPER,
	OP_MAKE_MULTIMETHOD,
	OP_MULTIMETHOD_APPEND,
	OP_MULTIMETHOD_REVERSE,
	NUMBER_OF_OPCODES,
};

extern char *opcodes_names[NUMBER_OF_OPCODES];

enum range_field {
	RANGE_FIELD_START = 0U,
	RANGE_FIELD_END = 1U,
	RANGE_FIELD_INCLUDE_START = 2U,
	RANGE_FIELD_INCLUDE_END = 3U,
	RANGE_FIELD_STEP = 4U,
};

typedef METHOD_RESULT (*VM_FUNC)(const VALUE *argv, VALUE *result);
typedef METHOD_RESULT (*VM_EXT_FUNC)(VM *vm, CTX *ctx, const VALUE *argv, VALUE *result);
void vm_init(VM *vm, int argc, char **argv);
size_t vm_load_bytecode(VM *vm, char *bc);
void ctx_init(CTX *ctx);
GLOBAL_VAR_INDEX check_global_index(VM *vm, const char *name, size_t name_len, int *found);
GLOBAL_VAR_INDEX get_global_index(VM *vm, const char *name, size_t name_len);

// typedef int VM_INT;
void set_global(VM *vm, const char *name, VALUE v);
METHOD_RESULT vm_run(VM *vm, CTX *ctx, IP ip, VALUE *result);
int vm_call_match_arg_to_type(VM *vm, CTX *ctx, VALUE arg, VALUE t, VALUE *result);
int vm_call_match_arg_to_any(VM *vm, CTX *ctx, VALUE arg, VALUE type_any, VALUE *result);
int vm_call_match_arg_to_pattern(VM *vm, CTX *ctx, VALUE arg, VALUE pattern, VALUE *result);
METHOD_RESULT vm_call(VM *vm, CTX *ctx, VALUE *result, VALUE callable, int argc, const VALUE *argv);
BYTECODE_HANDLE *ngs_create_bytecode();
void ngs_add_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE type, BYTECODE_SECTION_LEN len, char *data);
BYTECODE_HANDLE *ngs_start_unserializing_bytecode(char *data);
void ngs_fetch_bytecode_section(BYTECODE_HANDLE *h, BYTECODE_SECTION_TYPE *type, BYTECODE_SECTION_LEN *len, char **data);
// In obj.c
VALUE make_backtrace(VM *vm, CTX *ctx);
VALUE resolve_instruction_pointer(VM *vm, IP ip);
VALUE value_type(VM *vm, VALUE val);
int obj_is_of_type(VM *vm, VALUE obj, VALUE t);
#endif
