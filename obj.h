// NGS objects
#ifndef OBJ_H
#define OBJ_H
#include <stdint.h>
#include <stddef.h>
#include <dirent.h>
#include <ffi.h>
#include <pcre.h>

typedef uint16_t GLOBAL_VAR_INDEX;
#define GLOBAL_VAR_INDEX_FMT "%d"
typedef uint8_t LOCAL_VAR_INDEX;
typedef uint8_t UPVAR_INDEX;
typedef double NGS_REAL;
#define NGS_REAL_FMT "%.10f"
#define MAX_GLOBALS         (65535)
#define MAX_LOCALS            (255)
#define INITITAL_ARRAY_SIZE     (8)
#define MAX_INT_TO_STR_LEN    (256)
#define MAX_REAL_TO_STR_LEN    (256)

#define PARAMS_FLAG_ARR_SPLAT   (1)
#define PARAMS_FLAG_HASH_SPLAT  (2)
#define ADDITIONAL_PARAMS_COUNT (((params_flags & PARAMS_FLAG_ARR_SPLAT) != 0) + ((params_flags & PARAMS_FLAG_HASH_SPLAT) != 0))

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

typedef intptr_t VALUE_NUM;
#define VALUE_NUM_FMT PRIdPTR

typedef struct {
	VALUE type;
	VALUE val;
	VALUE attrs;
} OBJECT;

typedef struct {
	OBJECT base;
	size_t len;
	size_t allocated;
	size_t item_size;
} VAR_LEN_OBJECT;

typedef struct {
	OBJECT base;
	VALUE methods;
} MULTIMETHOD_OBJECT;

typedef struct {
	OBJECT base;
	NGS_REAL val;
} REAL_OBJECT;

typedef struct {
	OBJECT base;
	pthread_t val;
} PTHREAD_OBJECT;

typedef struct {
	OBJECT base;
	pthread_attr_t val;
} PTHREADATTR_OBJECT;

typedef struct {
	OBJECT base;
	pthread_mutex_t val;
} PTHREADMUTEX_OBJECT;

typedef struct {
	OBJECT base;
	pthread_mutexattr_t val;
} PTHREADMUTEXATTR_OBJECT;

typedef struct {
	OBJECT base;
} FFI_TYPE_OBJECT;

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
	HASH_OBJECT_ENTRY *entry;
} HASH_ENTRY_OBJECT;

typedef struct {
	OBJECT base;
	// Or maybe use base.val for the pointer?
	pcre *re;
} REGEXP_OBJECT;

typedef struct {
	OBJECT base; // Type points to the type, data to fields array
} USER_TYPE_INSTANCE_OBJECT;

typedef struct params {
	LOCAL_VAR_INDEX n_local_vars; // number of local variables including arguments
	LOCAL_VAR_INDEX n_params_required;
	LOCAL_VAR_INDEX n_params_optional;
	int flags;
	VALUE *params;
	// maybe not the best place but convenient and related to n_local_vars
	VALUE *locals;
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
	// base.val - NULL for normal types
	//          - type ID for built-in types
	VALUE name;
	VALUE fields; // Hash: name->index
	VALUE constructors; // Arr[F]
	VALUE parents; // Arr[NGS_TYPE]
	VALUE user; // Hash, user defined
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
	ffi_cif val;
} FFI_CIF_OBJECT;

typedef struct {
	OBJECT base;
	DIR *dir;
	int_fast8_t is_open;
} DIR_OBJECT;

// *** Add new ..._OBJECT typedefs above this line ***

// malloc() / NGS_MALLOC() memory is 8 bytes aligned, should be at least 4 bytes aligned
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
// (misc)
// 010110 10 - Kwargs marker
// (immediate types)
// XXXXXXXX 100000 10 - An Object


#define TAG_BITS    (2)
#define TAG_AND     (3)
#define TAG_INT     (1)

// TODO: Make sure it's correct on all architectures - start
#define NGS_INT_MIN (INTPTR_MIN >> TAG_BITS)
#define NGS_INT_MAX (INTPTR_MAX >> TAG_BITS)
#define NGS_RAND_MAX ((NGS_INT_MAX < RAND_MAX) ? NGS_INT_MAX : RAND_MAX)

#define NGS_INT_MIN_VALUE (VALUE){.num = ((INTPTR_MIN & ~TAG_AND) | TAG_INT)}
#define NGS_INT_MAX_VALUE (VALUE){.num = ((INTPTR_MAX & ~TAG_AND) | TAG_INT)}
// Make sure it's correct on all architectures - end

typedef enum {
	V_NULL   =  2,
	V_UNDEF  =  6,
	V_FALSE  = 10,
	V_TRUE   = 14,
	V_KWARGS_MARKER = 90,
} IMMEDIATE_VALUE;

#define MAX_VALUE_TAG_VALUE (V_KWARGS_MARKER >> TAG_BITS)
#define T_OBJ_TYPE_SHIFT_BITS (8)

typedef enum {
	T_OBJ           = 130,
	T_NULL          = 18,
	T_BOOL          = 22,
	T_INT           = 26,
	T_STR           = (1 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_ARR           = (2 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_FUN           = 38,
	T_ANY           = 42,
	T_SEQ           = 46,
	T_TYPE          = (3 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_HASH          = (4 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_CLIB          = (5 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_CSYM          = (6 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_NORMT         = 66,
	T_UTCTR         = (7 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_BASICT        = 74,
	T_BASICTI       = 78,
	T_NORMTI        = 82,
	T_REAL          = ( 8 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_PTHREAD       = ( 9 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_PTHREADATTR   = (10 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_PTHREADMUTEX  = (11 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_PTHREADMUTEXATTR = (12 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_NATIVE_METHOD = (13 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_CLOSURE       = (14 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_FFI_TYPE      = (15 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_FFI_CIF       = (16 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_REGEXP        = (17 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_DIR           = (18 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_MULMETHOD     = (19 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	T_LL_HASH_ENTRY = (20 << T_OBJ_TYPE_SHIFT_BITS) + T_OBJ,
	// *** Add new T_ itmes above this line ***
	// *** UPDATE MAX_T_OBJ_TYPE_ID ACCORDINGLY ***
} IMMEDIATE_TYPE;

#define MAX_T_OBJ_TYPE_ID (T_LL_HASH_ENTRY >> T_OBJ_TYPE_SHIFT_BITS)

// TODO: handle situation when n is wider than size_t - TAG_BITS bits
#define IS_NULL(v)      ((v).num == V_NULL)
#define IS_TRUE(v)      ((v).num == V_TRUE)
#define IS_FALSE(v)     ((v).num == V_FALSE)
#define IS_UNDEF(v)     ((v).num == V_UNDEF)
#define IS_NOT_UNDEF(v) ((v).num != V_UNDEF)
// Boolean 00001X10
#define IS_BOOL(v)      ((v.num & 0xFB) == 10)
#define IS_INT(v)       ((v.num & TAG_AND) == TAG_INT)
#define IS_KWARGS_MARKER(v)    ((v).num == V_KWARGS_MARKER)

#define SET_INT(v,n)    (v).num = ((n) << TAG_BITS) | TAG_INT
#define MAKE_INT(n)     ((VALUE){.num=((n) << TAG_BITS) | TAG_INT})
#define MAKE_BOOL(b)    ((VALUE){.num=((b) ? V_TRUE : V_FALSE)})
#define MAKE_OBJ(o)     ((VALUE){.ptr=(o)})
#define MAKE_NULL       ((VALUE){.num=V_NULL})
#define MAKE_UNDEF      ((VALUE){.num=V_UNDEF})
#define MAKE_FALSE      ((VALUE){.num=V_FALSE})
#define MAKE_TRUE       ((VALUE){.num=V_TRUE})
#define MAKE_KWARGS_MARKER     ((VALUE){.num=V_KWARGS_MARKER})
#define GET_INT(v)      ((v).num >> TAG_BITS)
#define GET_REAL(v)     (((REAL_OBJECT *) v.ptr)->val)
#define GET_PTHREAD(v)      (((PTHREAD_OBJECT *) v.ptr)->val)
#define GET_PTHREADATTR(v)  (((PTHREADATTR_OBJECT *) v.ptr)->val)
#define GET_PTHREADMUTEX(v) (((PTHREADMUTEX_OBJECT *) v.ptr)->val)
#define GET_PTHREADMUTEXATTR(v) (((PTHREADMUTEXATTR_OBJECT *) v.ptr)->val)
#define GET_FFI_TYPE(v) (((FFI_TYPE_OBJECT *) v.ptr)->base.val.ptr)
#define GET_FFI_CIF(v)  (((FFI_CIF_OBJECT *) v.ptr)->val)
#define SET_OBJ(v,o)    {(v).ptr = o; OBJ_ATTRS(v) = MAKE_NULL; }
#define SET_BOOL(v, b)  (v).num = b ? V_TRUE : V_FALSE

#define OBJ_LEN(v)                ((VAR_LEN_OBJECT *) (v).ptr)->len
#define OBJ_ALLOCATED(v)          ((VAR_LEN_OBJECT *) (v).ptr)->allocated
#define CLOSURE_OBJ_IP(v)         ((CLOSURE_OBJECT *) (v).ptr)->ip
#define CLOSURE_OBJ_N_LOCALS(v)   (((CLOSURE_OBJECT *) v.ptr)->params.n_local_vars)
#define CLOSURE_OBJ_N_REQ_PAR(v)  (((CLOSURE_OBJECT *) v.ptr)->params.n_params_required)
#define CLOSURE_OBJ_N_OPT_PAR(v)  (((CLOSURE_OBJECT *) v.ptr)->params.n_params_optional)
#define CLOSURE_OBJ_PARAMS(v)     (((CLOSURE_OBJECT *) v.ptr)->params.params)
#define CLOSURE_OBJ_LOCALS(v)     (((CLOSURE_OBJECT *) v.ptr)->params.locals)
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
#define NGS_TYPE_CONSTRUCTORS(v)  (((NGS_TYPE *)(v).ptr)->constructors)
#define NGS_TYPE_NAME(v)          (((NGS_TYPE *)(v).ptr)->name)
#define NGS_TYPE_ID(v)            (((NGS_TYPE *)(v).ptr)->base.val.num)
#define NGS_TYPE_FIELDS(v)        (((NGS_TYPE *)(v).ptr)->fields)
#define NGS_TYPE_PARENTS(v)       (((NGS_TYPE *)(v).ptr)->parents)
#define NGS_TYPE_USER(v)          (((NGS_TYPE *)(v).ptr)->user)
// TODO: rename OBJ_DATA to OBJ_VAL
#define OBJ_DATA(v)               (((OBJECT *)(v).ptr)->val)
#define OBJ_DATA_PTR(v)           (((OBJECT *)(v).ptr)->val.ptr)
#define OBJ_TYPE(v)               (((OBJECT *)(v).ptr)->type)
#define OBJ_ATTRS(v)              (((OBJECT *)(v).ptr)->attrs)
#define OBJ_TYPE_NUM(v)           (((OBJECT *)(v).ptr)->type.num)
#define OBJ_TYPE_PTR(v)           (((OBJECT *)(v).ptr)->type.ptr)
#define IS_OBJ(v)                 (((v).num & TAG_AND) == 0)
#define IS_STRING(v)              ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_STR)
#define IS_REAL(v)                ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_REAL)
#define IS_NATIVE_METHOD(v)       ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_NATIVE_METHOD)
#define IS_CLOSURE(v)             ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CLOSURE)
#define IS_ARRAY(v)               ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_ARR)
#define IS_NGS_TYPE(v)            ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_TYPE)
#define IS_BASIC_TYPE(v)          (IS_NGS_TYPE(v) && NGS_TYPE_ID(v))
#define IS_NORMAL_TYPE(v)         (IS_NGS_TYPE(v) && !NGS_TYPE_ID(v))
#define IS_NORMAL_TYPE_CONSTRUCTOR(v)           (((v.num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_UTCTR)
#define IS_NORMAL_TYPE_INSTANCE(v)((((v).num & TAG_AND) == 0) && ((OBJ_TYPE_NUM(v) & TAG_AND) == 0))
#define IS_BASIC_TYPE_INSTANCE(v) (!IS_NORMAL_TYPE_INSTANCE(v))
#define IS_VLO(v)                 (IS_ARRAY(v) || IS_STRING(v))
#define IS_HASH(v)                ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_HASH)
#define IS_CLIB(v)                ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CLIB)
#define IS_CSYM(v)                ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_CSYM)
#define IS_PTHREAD(v)             ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_PTHREAD)
#define IS_PTHREADATTR(v)         ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_PTHREADATTR)
#define IS_PTHREADMUTEX(v)        ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_PTHREADMUTEX)
#define IS_PTHREADMUTEXATTR(v)    ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_PTHREADMUTEXATTR)
#define IS_FFI_TYPE(v)            ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_FFI_TYPE)
#define IS_FFI_CIF(v)             ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_FFI_CIF)
#define IS_REGEXP(v)              ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_REGEXP)
#define IS_DIR(v)                 ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_DIR)
#define IS_MULMETHOD(v)           ((((v).num & TAG_AND) == 0) && OBJ_TYPE_NUM(v) == T_MULMETHOD)
// *** Add new IS_... macros above this line ***
#define ARRAY_ITEMS(v)            ((VALUE *)(OBJ_DATA_PTR(v)))
#define HASH_BUCKETS_N(v)         (((HASH_OBJECT *)(v).ptr)->n_buckets)
#define HASH_HEAD(v)              (((HASH_OBJECT *)(v).ptr)->head)
#define HASH_TAIL(v)              (((HASH_OBJECT *)(v).ptr)->tail)
#define NORMAL_TYPE_CONSTRUCTOR_TYPE(v)      OBJ_DATA(v)
#define NORMAL_TYPE_INSTANCE_TYPE(v)       OBJ_TYPE(v)
#define NORMAL_TYPE_INSTANCE_FIELDS(v)     OBJ_DATA(v)
#define REAL_OBJECT_VAL(v)        (((REAL_OBJECT *) (v).ptr)->val)
#define REGEXP_OBJECT_RE(v)       (((REGEXP_OBJECT *) (v).ptr)->re)
#define DIR_OBJECT_DIR(v)         (((DIR_OBJECT *) (v).ptr)->dir)
#define DIR_OBJECT_IS_OPEN(v)     (((DIR_OBJECT *) (v).ptr)->is_open)
#define MULTIMETHOD_METHODS(v)    (((MULTIMETHOD_OBJECT *) (v).ptr)->methods)
#define MULTIMETHOD_LEN(v)        (OBJ_LEN(MULTIMETHOD_METHODS(v)))
#define MULTIMETHOD_ITEMS(v)      (ARRAY_ITEMS(MULTIMETHOD_METHODS(v)))


// Boolean 00001X10
#define GET_INVERTED_BOOL(v)      ((VALUE){.num = (v).num ^= 4})

VALUE make_var_len_obj(uintptr_t type, const size_t item_size, const size_t len);
VALUE make_array(size_t len);
VALUE make_array_with_values(size_t len, const VALUE *values);
VALUE make_multimethod();
VALUE make_multimethod_with_value(const VALUE value);
VALUE make_multimethod_from_array(const VALUE arr);
VALUE make_hash(size_t start_buckets);
VALUE make_normal_type(VALUE name);
VALUE make_normal_type_constructor(VALUE normal_type);
VALUE make_normal_type_instance(VALUE normal_type);
METHOD_RESULT get_normal_type_instace_field(VALUE obj, VALUE field, VALUE *result);
void set_normal_type_instance_field(VALUE obj, VALUE field, VALUE v);
void add_type_inheritance(VALUE type, VALUE parent_type);
uint32_t hash(VALUE v);
HASH_OBJECT_ENTRY *get_hash_key(VALUE h, VALUE k);
void set_hash_key(VALUE h, VALUE k, VALUE v);
int del_hash_key(VALUE h, VALUE k);
void update_hash(VALUE dst, VALUE src);
VALUE make_string(const char *s);
VALUE make_string_of_len(const char *s, size_t len);
VALUE make_real(double n);
void vlo_ensure_additional_space(VALUE v, size_t n);
void array_push(VALUE arr, VALUE v);
void push_multimethod_method(VALUE multimethod, const VALUE method);
VALUE array_shift(VALUE arr);
void array_reverse(VALUE arr);
VALUE make_closure_obj(size_t ip, LOCAL_VAR_INDEX n_local_vars, LOCAL_VAR_INDEX n_params_required, LOCAL_VAR_INDEX n_params_optional, UPVAR_INDEX n_uplevels, int params_flags, VALUE *params, VALUE *locals);
VALUE join_strings(int argc, VALUE *argv);
// VALUE value_type(VM *vm, VALUE val);
// int obj_is_of_type(VM *vm, VALUE obj, VALUE t);
void dump(FILE *f, VALUE v);
void dump_titled(FILE *f, char *title, VALUE v);
char *obj_to_cstring(VALUE v);
char **obj_to_cstring_array(VALUE v);
METHOD_RESULT decode_json(VM *vm, VALUE s, VALUE *result);
METHOD_RESULT encode_json(VALUE obj, VALUE *result);
void *ngs_memmem(const void *haystack_start, size_t haystack_len, const void *needle_start, size_t needle_len);
char *ngs_strdup(const char *src);
char *ngs_strcat(const char *s1, const char *s2);
VALUE make_pthread();
VALUE make_pthread_attr();
VALUE make_pthread_mutex();
VALUE make_pthread_mutexattr();
VALUE make_ffi_type(ffi_type *t);
VALUE make_ffi_cif();
VALUE make_regexp();
VALUE make_DIR();
// *** Add new make_MYTPE(...) functions above this line ***

#endif
