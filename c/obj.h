// NGS objects
#ifndef OBJ_H
#define OBJ_H
#include <stdint.h>
#include <stddef.h>
#include <ffi.h>

typedef uint16_t GLOBAL_VAR_INDEX;
#define GLOBAL_VAR_INDEX_FMT "%d"
typedef uint8_t LOCAL_VAR_INDEX;
typedef uint8_t UPVAR_INDEX;
#define MAX_GLOBALS         (65535)
#define MAX_LOCALS            (255)
#define INITITAL_ARRAY_SIZE     (8)
#define MAX_INT_TO_STR_LEN    (256)
typedef uint16_t NATIVE_TYPE_ID;

#define PARAMS_FLAG_ARR_SPLAT   (1)
#define ADDITIONAL_PARAMS_COUNT ((params_flags & PARAMS_FLAG_ARR_SPLAT) != 0)

typedef enum {
	RESIZE_HASH_AFTER_SHRINK = 0,
	RESIZE_HASH_AFTER_GROW   = 1,
} RESIZE_HASH_AFTER;

// On problems with `intptr_t` change here according to Ruby source in `include/ruby/ruby.h`
// uintptr_t format for printf - PRIXPTR - printf("Blah %" PRIXPTR "\n", VALUE.num);
typedef union {
	intptr_t num;
	void * ptr;
} VALUE;

typedef struct {
	VALUE type;
	VALUE val;
} OBJECT;

typedef struct {
	OBJECT base;
	size_t len;
	size_t allocated;
	size_t item_size;
} VAR_LEN_OBJECT;

// https://www.igvita.com/2009/02/04/ruby-19-internals-ordered-hash/
typedef struct hash_object_entry {
	VALUE key;
	VALUE val;
	struct hash_object_entry *bucket_next;
	struct hash_object_entry *insertion_order_prev;
	struct hash_object_entry *insertion_order_next;
	uint32_t hash;
} HASH_OBJECT_ENTRY;

typedef struct {
	OBJECT base;
	size_t len;
	size_t n_buckets;
	HASH_OBJECT_ENTRY *head;
	HASH_OBJECT_ENTRY *tail;
} HASH_OBJECT;

typedef struct {
	OBJECT base;
	VALUE name;
	VALUE fields; // Hash: name->index
	VALUE constructors; // Arr[F]
	VALUE parents; // Arr[USER_TYPE_OBJECT]
	// TODO: maybe cached transitive parents
} USER_TYPE_OBJECT;

typedef struct {
	OBJECT base; // Type points to the type, data to fields array
} USER_TYPE_INSTANCE_OBJECT;

typedef struct params {
	LOCAL_VAR_INDEX n_local_vars; // number of local variables including arguments
	LOCAL_VAR_INDEX n_params_required;
	LOCAL_VAR_INDEX n_params_optional;
	int flags;
	VALUE *params;
} PARAMS_DESC;

typedef struct closure {
	OBJECT base;
	size_t ip;
	PARAMS_DESC params;
	VALUE **uplevels;
	UPVAR_INDEX n_uplevels;
} CLOSURE_OBJECT;

typedef struct native_method {
	OBJECT base;
	PARAMS_DESC params;
	int_fast8_t pass_extra_params;
} NATIVE_METHOD_OBJECT;

typedef struct ngs_type {
	OBJECT base;
	VALUE name;
	VALUE constructors;
	VALUE meta;
	NATIVE_TYPE_ID native_type_id;
} NGS_TYPE;

typedef struct {
	OBJECT base;
	VALUE name;
} CLIB_OBJECT;

typedef struct {
	OBJECT base;
	VALUE name;
	VALUE lib;
} CSYM_OBJECT;

typedef struct {
	OBJECT base;
	VALUE rtype;
	VALUE atypes;
	ffi_cif cif;
} CCIF_OBJECT;

// malloc() / NGS_MALLOC() memory is 8 bytes aligned, should be at least 4 bytes aligned
// .....000 - *OBJECT
// .....001 - int number
// OLD:
// .....000 - *OBJECT
// .....001 - int number
// .....010 - false
// .....011 - true
// .....100 - null
// .....101 - built_in_type (WIP)
// .....110 - reserved
// .....111 - undef
// NEW:
// ...... 00 - *OBJECT
// ...... 01 - int number
// XXXXXX 10 - misc values
// ...... 11 - reserved
// (scalar values)
// 000000 10 - null
// 000001 10 - undefined (aka undef)
// 00001X 10 - boolean
// 000010 10 - false
// 000011 10 - true
// (types)
// 000100 10 - Null
// 000101 10 - Bool
// 000110 10 - Int
// 000111 10 - Str
// 001000 10 - Arr
// 001001 10 - Fun
// 001010 10 - Any
// 001011 10 - Seq
// 001100 10 - Type


#define TAG_BITS    (2)
#define TAG_AND     (3)
#define TAG_INT     (1)
#define TYPE_AND (0xFF)

enum IMMEDIATE_VALUES {
	V_NULL  = 2,
	V_UNDEF = 6,
	V_FALSE = 10,
	V_TRUE  = 14,
	T_NULL  = 18,
	T_BOOL  = 22,
	T_INT   = 26,
	T_STR   = 30,
	T_ARR   = 34,
	T_FUN   = 38,
	T_ANY   = 42,
	T_SEQ   = 46,
	T_TYPE  = 50,
	T_HASH  = 54,
	T_CLIB  = 58,
	T_CSYM  = 62,
	T_UTYPE = 66,
	T_UTCTR = 70,
	T_NATIVE_METHOD = (1 << 8) | T_FUN,
	T_CLOSURE       = (2 << 8) | T_FUN,
};

// TODO: handle situation when n is wider than size_t - TAG_BITS bits
#define IS_NULL(v)      ((v).num == V_NULL)
#define IS_TRUE(v)      ((v).num == V_TRUE)
#define IS_FALSE(v)     ((v).num == V_FALSE)
#define IS_UNDEF(v)     ((v).num == V_UNDEF)
#define IS_NOT_UNDEF(v) ((v).num != V_UNDEF)
// Boolean 00001X10
#define IS_BOOL(v)      ((v.num & 0xFB) == 10)
#define IS_INT(v)       ((v.num & TAG_AND) == TAG_INT)

#define SET_INT(v,n)    (v).num = ((n) << TAG_BITS) | TAG_INT
#define MAKE_INT(n)     ((VALUE){.num=((n) << TAG_BITS) | TAG_INT})
#define MAKE_BOOL(b)    ((VALUE){.num=((b) ? V_TRUE : V_FALSE)})
#define MAKE_OBJ(o)     ((VALUE){.ptr=(o)})
#define GET_INT(v)      ((v).num >> TAG_BITS)
#define SET_OBJ(v,o)    (v).ptr = o
#define SET_NULL(v)     (v).num = V_NULL
#define SET_FALSE(v)    (v).num = V_FALSE
#define SET_TRUE(v)     (v).num = V_TRUE
#define SET_BOOL(v, b)  (v).num = b ? V_TRUE : V_FALSE
#define SET_UNDEF(v)    (v).num = V_UNDEF

#define OBJ_LEN(v)                ((VAR_LEN_OBJECT *) v.ptr)->len
#define OBJ_ALLOCATED(v)          ((VAR_LEN_OBJECT *) v.ptr)->allocated
#define CLOSURE_OBJ_IP(v)         ((CLOSURE_OBJECT *) v.ptr)->ip
#define CLOSURE_OBJ_N_LOCALS(v)   (((CLOSURE_OBJECT *) v.ptr)->params.n_local_vars)
#define CLOSURE_OBJ_N_REQ_PAR(v)  (((CLOSURE_OBJECT *) v.ptr)->params.n_params_required)
#define CLOSURE_OBJ_N_OPT_PAR(v)  (((CLOSURE_OBJECT *) v.ptr)->params.n_params_optional)
#define CLOSURE_OBJ_PARAMS(v)     (((CLOSURE_OBJECT *) v.ptr)->params.params)
#define CLOSURE_OBJ_N_UPLEVELS(v) (((CLOSURE_OBJECT *) v.ptr)->n_uplevels)
#define CLOSURE_OBJ_UPLEVELS(v)   (((CLOSURE_OBJECT *) v.ptr)->uplevels)
#define CLOSURE_OBJ_PARAMS_FLAGS(v) (((CLOSURE_OBJECT *) v.ptr)->params.flags)
#define CLIB_OBJECT_NAME(v)       ((CLIB_OBJECT *) v.ptr)->name
#define CSYM_OBJECT_NAME(v)       ((CSYM_OBJECT *) v.ptr)->name
#define CSYM_OBJECT_LIB(v)        ((CSYM_OBJECT *) v.ptr)->lib
#define NATIVE_METHOD_OBJ_N_REQ_PAR(v)  ((NATIVE_METHOD_OBJECT *) v.ptr)->params.n_params_required
#define NATIVE_METHOD_OBJ_N_OPT_PAR(v)  ((NATIVE_METHOD_OBJECT *) v.ptr)->params.n_params_optional
#define NATIVE_METHOD_OBJ_PARAMS(v)     (((NATIVE_METHOD_OBJECT *) v.ptr)->params.params)
#define NATIVE_METHOD_EXTRA_PARAMS(v)   (((NATIVE_METHOD_OBJECT *) v.ptr)->pass_extra_params)
#define NGS_TYPE_CONSTRUCTORS(v)  ((NGS_TYPE *) v.ptr)->constructors
#define NGS_TYPE_NAME(v)          ((NGS_TYPE *) v.ptr)->name
#define NGS_TYPE_ID(v)            ((NGS_TYPE *) v.ptr)->native_type_id
// TODO: reanme OBJ_DATA to OBJ_VAL
#define OBJ_DATA(v)               (((OBJECT *)(v).ptr)->val)
#define OBJ_DATA_PTR(v)           (((OBJECT *)(v).ptr)->val.ptr)
#define OBJ_TYPE(v)               (((OBJECT *)(v).ptr)->type)
#define OBJ_TYPE_NUM(v)           (((OBJECT *)(v).ptr)->type.num)
#define OBJ_TYPE_PTR(v)           (((OBJECT *)(v).ptr)->type.ptr)
#define IS_OBJ(v)                 ((v.num & TAG_AND) == 0)
#define IS_STRING(v)              (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_STR)
#define IS_NATIVE_METHOD(v)       (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_NATIVE_METHOD)
#define IS_CLOSURE(v)             (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CLOSURE)
#define IS_ARRAY(v)               (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_ARR)
#define IS_NGS_TYPE(v)            (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_TYPE)
#define IS_USER_TYPE(v)           (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_UTYPE)
#define IS_USERT_CTR(v)           (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_UTCTR)
#define IS_USERT_INST(v)          (((v.num & TAG_AND) == 0) && ((OBJ_TYPE_NUM(v) & TAG_AND) == 0))
#define IS_VLO(v)                 (IS_ARRAY(v) || IS_STRING(v))
#define IS_HASH(v)                (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_HASH)
#define IS_CLIB(v)                (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CLIB)
#define IS_CSYM(v)                (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CSYM)
#define ARRAY_ITEMS(v)            ((VALUE *)(OBJ_DATA_PTR(v)))
#define HASH_BUCKETS_N(v)         (((HASH_OBJECT *)(v).ptr)->n_buckets)
#define HASH_HEAD(v)              (((HASH_OBJECT *)(v).ptr)->head)
#define HASH_TAIL(v)              (((HASH_OBJECT *)(v).ptr)->tail)
#define UT_NAME(v)                (((USER_TYPE_OBJECT *)(v).ptr)->name)
#define UT_FIELDS(v)              (((USER_TYPE_OBJECT *)(v).ptr)->fields)
#define UT_CONSTRUCTORS(v)        (((USER_TYPE_OBJECT *)(v).ptr)->constructors)
#define UT_CONSTRUCTOR_UT(v)      OBJ_DATA(v)
#define UT_INSTANCE_TYPE(v)       OBJ_TYPE(v)
#define UT_INSTANCE_FIELDS(v)     OBJ_DATA(v)

// Boolean 00001X10
#define GET_INVERTED_BOOL(v)      ((VALUE){.num = (v).num ^= 4})

VALUE make_var_len_obj(uintptr_t type, const size_t item_size, const size_t len);
VALUE make_array(size_t len);
VALUE make_array_with_values(size_t len, const VALUE *values);
VALUE make_hash(size_t start_buckets);
VALUE make_user_type(VALUE name);
VALUE make_user_type_constructor(VALUE user_type);
VALUE make_user_type_instance(VALUE user_type);
METHOD_RESULT get_user_type_instace_attribute(VALUE obj, VALUE attr, VALUE *result);
void set_user_type_instance_attribute(VALUE obj, VALUE attr, VALUE v);
uint32_t hash(VALUE v);
HASH_OBJECT_ENTRY *get_hash_key(VALUE h, VALUE k);
void set_hash_key(VALUE h, VALUE k, VALUE v);
int del_hash_key(VALUE h, VALUE k);
VALUE make_string(const char *s);
VALUE make_string_of_len(const char *s, size_t len);
void vlo_ensure_additional_space(VALUE v, size_t n);
void array_push(VALUE arr, VALUE v);
VALUE array_shift(VALUE arr);
void array_reverse(VALUE arr);
VALUE make_closure_obj(size_t ip, LOCAL_VAR_INDEX n_local_vars, LOCAL_VAR_INDEX n_params_required, LOCAL_VAR_INDEX n_params_optional, UPVAR_INDEX n_uplevels, int params_flags, VALUE *params);
VALUE join_strings(int argc, VALUE *argv);
int obj_is_of_type(VALUE obj, VALUE t);
void dump(VALUE v);
void dump_titled(char *title, VALUE v);
char *obj_to_cstring(VALUE v);

#endif
