#include <assert.h>
#include <execinfo.h>
#include <inttypes.h>
#include <string.h>

#include <json-c/json.h>

#include "ngs.h"
#include "obj.h"
#include "vm.h"

static void _dump(VALUE v, int level) {
	char **symbols;
	void *symbols_buffer[1];
	VALUE *ptr;
	size_t i;
	HASH_OBJECT_ENTRY *e;
	HASH_OBJECT_ENTRY **buckets;

	if(IS_NULL(v))  { printf("%*s* null\n",    level << 1, ""); goto exit; }
	if(IS_TRUE(v))  { printf("%*s* true\n",    level << 1, ""); goto exit; }
	if(IS_FALSE(v)) { printf("%*s* false\n",   level << 1, ""); goto exit; }
	if(IS_UNDEF(v)) { printf("%*s* undef\n",   level << 1, ""); goto exit; }

	if(IS_INT(v))   { printf("%*s* int %" VALUE_NUM_FMT "\n", level << 1, "", GET_INT(v)); goto exit; }
	if(IS_REAL(v))  { printf("%*s* real %g\n", level << 1, "", REAL_OBJECT_VAL(v)); goto exit; }

	if(IS_STRING(v)) {
		// TODO: properly handle
		//       1. non-printable characters
		//       2. zero character
		printf("%*s* string(len=%zu) %.*s\n", level << 1, "", OBJ_LEN(v), (int) OBJ_LEN(v), (char *)OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_NATIVE_METHOD(v)) {
		symbols_buffer[0] = OBJ_DATA_PTR(v);
		symbols = backtrace_symbols(symbols_buffer, 1);
		printf("%*s* native method %s at %p req_params=%d\n", level << 1, "", symbols[0], OBJ_DATA_PTR(v), NATIVE_METHOD_OBJ_N_REQ_PAR(v));
		for(i=0; i<NATIVE_METHOD_OBJ_N_REQ_PAR(v); i++) {
			printf("%*s* required parameter %zu\n", (level+1) << 1, "", i+1);
			_dump(NATIVE_METHOD_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(NATIVE_METHOD_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		goto exit;
	}

	if(IS_CLOSURE(v)) {
		printf("%*s* closure ip=%zu locals_including_params=%d req_params=%d opt_params=%d n_uplevels=%d params_flags=%d\n", level << 1, "",
			CLOSURE_OBJ_IP(v),
			CLOSURE_OBJ_N_LOCALS(v),
			CLOSURE_OBJ_N_REQ_PAR(v),
			CLOSURE_OBJ_N_OPT_PAR(v),
			CLOSURE_OBJ_N_UPLEVELS(v),
			CLOSURE_OBJ_PARAMS_FLAGS(v)
		);
		for(i=0; i<CLOSURE_OBJ_N_REQ_PAR(v); i++) {
			printf("%*s* required parameter %zu\n", (level+1) << 1, "", i+1);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		if(CLOSURE_OBJ_N_OPT_PAR(v)) {
			printf("%*s* dumping optional parameters is not implemented yet\n", (level+1) << 1, "");
		}
		if(CLOSURE_OBJ_PARAMS_FLAGS(v) & PARAMS_FLAG_ARR_SPLAT) {
			printf("%*s* array splat parameter %zu\n", (level+1) << 1, "", i+1);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		goto exit;
	}

	if(IS_ARRAY(v)) {
		printf("%*s* array of length %zu\n", level << 1, "", OBJ_LEN(v));
		for(i=0, ptr=(VALUE *)OBJ_DATA_PTR(v); i<OBJ_LEN(v); i++, ptr++) {
			_dump(*ptr, level+1);
		}
		goto exit;
	}

	if(IS_HASH(v)) {
		printf("%*s* hash with total of %zu items in %zu buckets at %p\n", level << 1, "", OBJ_LEN(v), HASH_BUCKETS_N(v), OBJ_DATA_PTR(v));
		buckets = OBJ_DATA_PTR(v);
		for(i=0; i<HASH_BUCKETS_N(v); i++) {
			if(!buckets[i]) { continue; }
			printf("%*s* bucket # %zu\n", (level+1) << 1, "", i);
			for(e=buckets[i]; e; e=e->bucket_next) {
				printf("%*s* item at %p with hash() of %u insertion_order_prev=%p insertion_order_next=%p \n", (level+2) << 1, "", e, e->hash, e->insertion_order_prev, e->insertion_order_next);
				printf("%*s* key\n", (level+3) << 1, "");
				_dump(e->key, level+4);
				printf("%*s* value\n", (level+3) << 1, "");
				_dump(e->val, level+4);
			}
		}
		goto exit;
	}

	if(IS_NGS_TYPE(v)) {
		printf("%*s* type (name and optionally constructors and parents follow) id=%" PRIdPTR " ptr=%p\n", level << 1, "", NGS_TYPE_ID(v), IS_NORMAL_TYPE(v) ? v.ptr : 0);
		_dump(NGS_TYPE_NAME(v), level + 1);
		if(level < 3) {
			_dump(NGS_TYPE_FIELDS(v), level + 1);
			_dump(NGS_TYPE_CONSTRUCTORS(v), level + 1);
			_dump(NGS_TYPE_PARENTS(v), level + 1);
		}
		goto exit;
	}

	if(IS_CLIB(v)) {
		printf("%*s* C library (name follows) ptr=%p\n", level << 1, "", OBJ_DATA_PTR(v));
		_dump(CLIB_OBJECT_NAME(v), level + 1);
		goto exit;
	}

	if(IS_CSYM(v)) {
		printf("%*s* C symbol (name and libraray follow) ptr=%p\n", level << 1, "", OBJ_DATA_PTR(v));
		_dump(CSYM_OBJECT_NAME(v), level + 1);
		_dump(CSYM_OBJECT_LIB(v), level + 1);
		goto exit;
	}

	if(IS_NORMAL_TYPE_CONSTRUCTOR(v)) {
		printf("%*s* user type constructor (type optionally follows)\n", level << 1, "");
		if(level < 3) {
			_dump(OBJ_DATA(v), level + 1);
		}
		goto exit;
	}

	if(IS_NORMAL_TYPE_INSTANCE(v)) {
		printf("%*s* user type instance (type and fields optionally follow)\n", level << 1, "");
		// level < 4 so that uncaught exception ImplNotFound could display the type of the arguments
		if(level < 4) {
			_dump(NORMAL_TYPE_INSTANCE_TYPE(v), level + 1);
			_dump(NORMAL_TYPE_INSTANCE_FIELDS(v), level + 1);
		}
		goto exit;
	}

	printf("%*s* (dump not implemented for the object at %p)\n", level << 1, "", OBJ_DATA_PTR(v));

exit:
	return;
}

// TODO: consider allocating power-of-two length
VALUE make_var_len_obj(uintptr_t type, const size_t item_size, const size_t len) {

	VALUE v;
	VAR_LEN_OBJECT *vlo;

	vlo = NGS_MALLOC(sizeof(*vlo));
	assert(vlo);
	vlo->base.type.num = type;
	vlo->len = len;
	vlo->allocated = len;
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

VALUE make_array(size_t len) {
	VALUE ret;
	ret = make_var_len_obj(T_ARR, sizeof(VALUE), len);
	return ret;
}

VALUE make_array_with_values(size_t len, const VALUE *values) {
	VALUE ret;
	ret = make_array(len);
	memcpy(OBJ_DATA_PTR(ret), values, sizeof(VALUE)*len);
	return ret;
}

VALUE make_hash(size_t start_buckets) {
	VALUE ret;
	HASH_OBJECT *hash;
	hash = NGS_MALLOC(sizeof(*hash));
	assert(hash);

	SET_OBJ(ret, hash);
	OBJ_TYPE_NUM(ret) = T_HASH;

	if(start_buckets) {
		OBJ_DATA_PTR(ret) = NGS_MALLOC(start_buckets * sizeof(HASH_OBJECT_ENTRY *));
		memset(OBJ_DATA_PTR(ret), 0, start_buckets * sizeof(HASH_OBJECT_ENTRY *)); // XXX check if needed
	} else {
		OBJ_DATA_PTR(ret) = NULL;
	}
	HASH_BUCKETS_N(ret) = start_buckets;
	HASH_HEAD(ret) = NULL;
	HASH_TAIL(ret) = NULL;
	OBJ_LEN(ret) = 0;

	return ret;
}

VALUE make_normal_type(VALUE name) {
	VALUE ret;
	NGS_TYPE *t;
	t = NGS_MALLOC(sizeof(*t));
	assert(t);

	SET_OBJ(ret, t);
	OBJ_TYPE_NUM(ret) = T_TYPE;

	NGS_TYPE_NAME(ret) = name;
	NGS_TYPE_FIELDS(ret) = make_hash(8); // Hash: name->index
	NGS_TYPE_CONSTRUCTORS(ret) = make_array(1);
	NGS_TYPE_PARENTS(ret) = make_array(0);

	VALUE ctr = make_normal_type_constructor(ret);
	ARRAY_ITEMS(NGS_TYPE_CONSTRUCTORS(ret))[0] = ctr;

	return ret;
}

VALUE make_normal_type_constructor(VALUE normal_type) {
	VALUE ret;
	OBJECT *normal_type_constructor;
	normal_type_constructor = NGS_MALLOC(sizeof(*normal_type_constructor));
	assert(normal_type_constructor);

	SET_OBJ(ret, normal_type_constructor);
	OBJ_TYPE_NUM(ret) = T_UTCTR;
	NORMAL_TYPE_CONSTRUCTOR_TYPE(ret) = normal_type;

	return ret;
}

VALUE make_normal_type_instance(VALUE normal_type) {

	VALUE ret;
	USER_TYPE_INSTANCE_OBJECT *normal_type_instance;
	normal_type_instance = NGS_MALLOC(sizeof(*normal_type_instance));
	assert(normal_type_instance);

	SET_OBJ(ret, normal_type_instance);
	OBJ_TYPE(ret) = normal_type;
	OBJ_DATA(ret) = make_array(0);

	return ret;
};

METHOD_RESULT get_normal_type_instace_attribute(VALUE obj, VALUE attr, VALUE *result) {
	VALUE ut;
	HASH_OBJECT_ENTRY *e;
	size_t n;
	ut = NORMAL_TYPE_INSTANCE_TYPE(obj);
	e = get_hash_key(NGS_TYPE_FIELDS(ut), attr);
	if(!e) {
		return METHOD_EXCEPTION;
	}
	n = GET_INT(e->val);
	if(n >= OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(obj))) {
		return METHOD_EXCEPTION;
	}
	*result = ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(obj))[n];
	if(IS_UNDEF(*result)) {
		return METHOD_EXCEPTION;
	}
	return METHOD_OK;
};

void set_normal_type_instance_attribute(VALUE obj, VALUE attr, VALUE v) {
	VALUE ut;
	HASH_OBJECT_ENTRY *e;
	size_t n;
	ut = NORMAL_TYPE_INSTANCE_TYPE(obj);
	e = get_hash_key(NGS_TYPE_FIELDS(ut), attr);
	if(e) {
		n = GET_INT(e->val);
	} else {
		n = OBJ_LEN(NGS_TYPE_FIELDS(ut));
		set_hash_key(NGS_TYPE_FIELDS(ut), attr, MAKE_INT(n));
	}
	// TODO: more optimized
	while(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(obj)) < n) {
		array_push(NORMAL_TYPE_INSTANCE_FIELDS(obj), (VALUE){.num = V_UNDEF});
	}
	if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(obj)) == n) {
		array_push(NORMAL_TYPE_INSTANCE_FIELDS(obj), v);
		return;
	}
	ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(obj))[n] = v;
};

void add_normal_type_inheritance(VALUE type, VALUE parent_type) {
	assert(IS_NORMAL_TYPE(type));
	assert(IS_NORMAL_TYPE(parent_type));
	array_push(NGS_TYPE_PARENTS(type), parent_type);
	// TODO: check for circularity (and/or depth?)
	// TODO: invalidate cached "is" results for the types involved, when such caching is implemented
}

// TODO: implement comparison of the rest of the types
int is_equal(VALUE a, VALUE b) {
	if(IS_INT(a) && IS_INT(b)) {
		return GET_INT(a) == GET_INT(b);
	}
	if(IS_STRING(a) && IS_STRING(b)) {
		if(OBJ_LEN(a) != OBJ_LEN(b)) {
			return 0;
		}
		return !memcmp(OBJ_DATA_PTR(a), OBJ_DATA_PTR(b), OBJ_LEN(a));
	}
	if(IS_OBJ(a) && IS_OBJ(b)) {
		return a.num == b.num;
	}
	return 0;
}

// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
// http://www.isthe.com/chongo/tech/comp/fnv/index.html#FNV-param
#define FNV_PRIME (16777619U)
#define FNV_OFFSET_BASIS (2166136261U)
uint32_t hash(VALUE v) {
	uint32_t ret;
	union {
		uint32_t i;
		unsigned char c[4];
	} t;
	if(IS_INT(v)) {
		// XXX: check how exactly the casting is done
		return (uint32_t)(GET_INT(v));
	}
	if(IS_STRING(v)) {
		unsigned char *c;
		size_t i, len=OBJ_LEN(v);
		ret = FNV_OFFSET_BASIS;
		for(i=0, c=OBJ_DATA_PTR(v); i<len; i++, c++) {
			ret *= FNV_PRIME;
			ret ^= *c;
		}
		return ret;
	}
	if(IS_OBJ(v)) {
		// Warning: only using lower 32 significant bits out of 60 significan bits on x86_64.
		// Observed to be 16 bytes aligned on x86_64 Linux. Discarding zero bits.
		t.i = v.num >> 4;
		ret = FNV_OFFSET_BASIS;
		ret *= FNV_PRIME;
		ret ^= t.c[3];
		ret *= FNV_PRIME;
		ret ^= t.c[2];
		ret *= FNV_PRIME;
		ret ^= t.c[1];
		ret *= FNV_PRIME;
		ret ^= t.c[0];
		return ret;
	}
	// XXX: to implement hashing of the rest
	// Not sure whether to degrade performance by returning a constant or abort here or warn.
	return 1020304050;
}

void resize_hash_for_new_len(VALUE h, RESIZE_HASH_AFTER after) {

	size_t new_buckets_n;
	uint32_t n;
	HASH_OBJECT_ENTRY *e;
	HASH_OBJECT_ENTRY **buckets;

	new_buckets_n = HASH_BUCKETS_N(h);

	if(after == RESIZE_HASH_AFTER_GROW) {
		if(new_buckets_n == 0) {
			new_buckets_n = 8;
		}
		while(new_buckets_n < OBJ_LEN(h)) {
			new_buckets_n <<= 1;
		}
	} else {
		while(new_buckets_n > OBJ_LEN(h)) {
			new_buckets_n >>= 1;
		}
	}
	if(HASH_BUCKETS_N(h) == new_buckets_n) {
		return;
	}

	if(!new_buckets_n) {
		OBJ_DATA_PTR(h) = NULL;
		return;
	}

	buckets = NGS_MALLOC(new_buckets_n * sizeof(HASH_OBJECT_ENTRY *));
	assert(buckets);
	memset(buckets, 0, new_buckets_n * sizeof(HASH_OBJECT_ENTRY *));
	for(e=HASH_HEAD(h); e; e=e->insertion_order_next) {
		n = e->hash % new_buckets_n;
		e->bucket_next = buckets[n];
		buckets[n] = e;
	}
	OBJ_DATA_PTR(h) = buckets;
	HASH_BUCKETS_N(h) = new_buckets_n;
}

HASH_OBJECT_ENTRY *get_hash_key(VALUE h, VALUE k) {
	HASH_OBJECT_ENTRY *e;
	HASH_OBJECT_ENTRY **buckets = OBJ_DATA_PTR(h);
	uint32_t n;
	assert(IS_HASH(h));
	if(!OBJ_LEN(h)) {
		return NULL;
	}
	n = hash(k) % HASH_BUCKETS_N(h);
	for(e=buckets[n]; e; e=e->bucket_next) {
		if(is_equal(e->key, k)) {
			return e;
		}
	}
	return NULL;
}

void set_hash_key(VALUE h, VALUE k, VALUE v) {
	HASH_OBJECT_ENTRY *e;
	HASH_OBJECT_ENTRY **buckets;
	uint32_t n;

	e = get_hash_key(h, k);

	if(e) {
		e->val = v;
		return;
	}

	OBJ_LEN(h)++;
	resize_hash_for_new_len(h, RESIZE_HASH_AFTER_GROW);
	buckets = OBJ_DATA_PTR(h);
	e = NGS_MALLOC(sizeof(*e));
	assert(e);
	e->hash = hash(k);
	n = e->hash % HASH_BUCKETS_N(h);
	e->key = k;
	e->val = v;
	e->bucket_next = buckets[n];
	e->insertion_order_prev = HASH_TAIL(h);
	e->insertion_order_next = NULL;
	buckets[n] = e;
	if(!HASH_HEAD(h)) {
		HASH_HEAD(h) = e;
	}
	if(HASH_TAIL(h)) {
		HASH_TAIL(h)->insertion_order_next = e;
	}
	HASH_TAIL(h) = e;

}

int del_hash_key(VALUE h, VALUE k) {
	HASH_OBJECT_ENTRY *e, **prev;
	HASH_OBJECT_ENTRY **buckets = OBJ_DATA_PTR(h);
	uint32_t n = hash(k) % HASH_BUCKETS_N(h);
	for(e=buckets[n], prev=&buckets[n]; e; prev=&e->bucket_next, e=e->bucket_next) {
		if(is_equal(e->key, k)) {
			if(HASH_HEAD(h) == e) {
				HASH_HEAD(h) = e->insertion_order_next;
			}
			if(HASH_TAIL(h) == e) {
				HASH_TAIL(h) = e->insertion_order_prev;
			}
			if(e->insertion_order_prev) {
				e->insertion_order_prev->insertion_order_next = e->insertion_order_next;
			}
			if(e->insertion_order_next) {
				e->insertion_order_next->insertion_order_prev = e->insertion_order_prev;
			}
			*prev = e->bucket_next;
			OBJ_LEN(h)--;
			resize_hash_for_new_len(h, RESIZE_HASH_AFTER_SHRINK);
			return 1;
		}
	}
	return 0;
}

VALUE make_string(const char *s) {
	VALUE v;
	VAR_LEN_OBJECT *vlo;
	vlo = NGS_MALLOC(sizeof(*vlo));
	vlo->len = strlen(s);
	vlo->base.type.num = T_STR;
	vlo->base.val.ptr = NGS_MALLOC_ATOMIC(vlo->len);
	memcpy(vlo->base.val.ptr, s, vlo->len);
	SET_OBJ(v, vlo);
	return v;
}

VALUE make_string_of_len(const char *s, size_t len) {
	VALUE v;
	VAR_LEN_OBJECT *vlo;
	vlo = NGS_MALLOC(sizeof(*vlo));
	vlo->len = len;
	vlo->base.type.num = T_STR;
	vlo->base.val.ptr = NGS_MALLOC_ATOMIC(vlo->len);
	if(s) {
		memcpy(vlo->base.val.ptr, s, vlo->len);
	}
	SET_OBJ(v, vlo);
	return v;
}

VALUE make_real(double n) {
	VALUE v;
	REAL_OBJECT *r;
	r = NGS_MALLOC(sizeof(*r));
	r->base.type.num = T_REAL;
	SET_OBJ(v, r);
	REAL_OBJECT_VAL(v) = n;
	return v;
}


// Very not thread safe
// Inspired by utarray.h
void vlo_ensure_additional_space(VALUE v, size_t n) {
	VAR_LEN_OBJECT *o;
	assert(IS_VLO(v));
	o = v.ptr;
	if(o->allocated - o->len < n) {
		if(!o->allocated) {
			o->allocated = INITITAL_ARRAY_SIZE;
		}
		while(o->allocated - o->len < n) {
			o->allocated <<= 1;
		}
		o->base.val.ptr = NGS_REALLOC(o->base.val.ptr, o->allocated * o->item_size);
		assert(o->base.val.ptr);
	}
}

void array_push(VALUE arr, VALUE v) {
	VAR_LEN_OBJECT *o;
	VALUE *arr_items;
	vlo_ensure_additional_space(arr, 1);
	o = arr.ptr;
	arr_items = o->base.val.ptr;
	arr_items[o->len++] = v;
}

// TODO: shring allocated memory
VALUE array_shift(VALUE arr) {
	VALUE ret;
	assert(OBJ_LEN(arr));
	ret = ARRAY_ITEMS(arr)[0];
	OBJ_LEN(arr)--;
	memmove(&ARRAY_ITEMS(arr)[0], &ARRAY_ITEMS(arr)[1], OBJ_LEN(arr)*sizeof(ARRAY_ITEMS(arr)[0]));
	return ret;
}

// TODO: expose as native_reverse_arr() ?
//       It's in-place, not sure about exposing.
void array_reverse(VALUE arr) {
	VALUE *p1, *p2, tmp;
	assert(IS_ARRAY(arr));
	if(OBJ_LEN(arr) < 2) {
		return;
	}
	for(p1 = &ARRAY_ITEMS(arr)[0], p2 = &ARRAY_ITEMS(arr)[OBJ_LEN(arr)-1]; p1 < p2; p1++, p2--) {
		tmp = *p1;
		*p1 = *p2;
		*p2 = tmp;
	}
}

VALUE make_closure_obj(size_t ip, LOCAL_VAR_INDEX n_local_vars, LOCAL_VAR_INDEX n_params_required, LOCAL_VAR_INDEX n_params_optional, UPVAR_INDEX n_uplevels, int params_flags, VALUE *params) {

	VALUE v;
	CLOSURE_OBJECT *c;
	size_t params_size;

	c = NGS_MALLOC(sizeof(*c));
	assert(c);
	c->base.type.num = T_CLOSURE;
	c->ip = ip;
	c->params.n_local_vars = n_local_vars;
	c->params.n_params_required = n_params_required;
	c->params.n_params_optional = n_params_optional;
	c->params.flags = params_flags;
	params_size = ((n_params_required + ADDITIONAL_PARAMS_COUNT)*2 + n_params_optional*3) * sizeof(VALUE);
	c->params.params = NGS_MALLOC(params_size);
	assert(c->params.params);
	memcpy(c->params.params, params, params_size);
	c->n_uplevels = n_uplevels;

	SET_OBJ(v, c);

	return v;
}

VALUE join_strings(int argc, VALUE *argv) {
	size_t len;
	int i;
	VALUE ret;
	void *dst;

	// printf("JOIN ARGC %d\n", argc);
	for(i=0, len=0; i<argc; i++) {
		// dump_titled("JOIN", argv[i]);
		assert(IS_STRING(argv[i]));
		len += OBJ_LEN(argv[i]);
	}
	// printf("JOIN TOTAL LEN %d\n", len);
	ret = make_var_len_obj(T_STR, 1, len);
	for(i=0, dst=OBJ_DATA_PTR(ret); i<argc; i++) {
		len = OBJ_LEN(argv[i]);
		// printf("JOIN ITEM LEN %d\n", len);
		memcpy(dst, OBJ_DATA_PTR(argv[i]), len);
		dst += len;
	}
	// dump_titled("JOIN RET", ret);
	return ret;
}

#define OBJ_C_OBJ_IS_OF_TYPE(type, check) if(tid == type) { return check(obj); }

int ut_is_ut(VALUE ut_child, VALUE ut_parent) {
	if(ut_child.ptr == ut_parent.ptr) { return 1; }
	size_t len, i;
	for(i=0, len=OBJ_LEN(NGS_TYPE_PARENTS(ut_child)); i<len; i++) {
		if(ut_is_ut(ARRAY_ITEMS(NGS_TYPE_PARENTS(ut_child))[i], ut_parent)) {
			return 1;
		}
	}
	return 0;
}

// TODO: make it faster, probably using vector of NATIVE_TYPE_IDs and how to detect them
//       maybe re-work tagged types so the check would be VALUE & TYPE_VAL == TYPE_VAL
// TODO: profiling before optimizations
int obj_is_of_type(VALUE obj, VALUE t) {
	VALUE_NUM tid;
	assert(IS_NGS_TYPE(t));
	tid = NGS_TYPE_ID(t);
	if(tid) {
		if(tid == T_ANY) { return 1; }
		OBJ_C_OBJ_IS_OF_TYPE(T_NULL, IS_NULL);
		OBJ_C_OBJ_IS_OF_TYPE(T_BOOL, IS_BOOL);
		OBJ_C_OBJ_IS_OF_TYPE(T_INT, IS_INT);
		OBJ_C_OBJ_IS_OF_TYPE(T_STR, IS_STRING);
		OBJ_C_OBJ_IS_OF_TYPE(T_ARR, IS_ARRAY);
		OBJ_C_OBJ_IS_OF_TYPE(T_TYPE, IS_NGS_TYPE);
		OBJ_C_OBJ_IS_OF_TYPE(T_HASH, IS_HASH);
		OBJ_C_OBJ_IS_OF_TYPE(T_NORMTI, IS_NORMAL_TYPE_INSTANCE);
		OBJ_C_OBJ_IS_OF_TYPE(T_BASICTI, IS_BASIC_TYPE_INSTANCE);
		OBJ_C_OBJ_IS_OF_TYPE(T_CLIB, IS_CLIB);
		OBJ_C_OBJ_IS_OF_TYPE(T_CSYM, IS_CSYM);
		if(tid == T_FUN) {
			if(IS_ARRAY(obj)) {
				if(OBJ_LEN(obj)) {
					return obj_is_of_type(ARRAY_ITEMS(obj)[0], t);
				} else {
					return 0;
				}
			}
			return IS_NATIVE_METHOD(obj) || IS_CLOSURE(obj) || IS_NGS_TYPE(obj);
		}
		OBJ_C_OBJ_IS_OF_TYPE(T_NORMT, IS_NORMAL_TYPE);
		OBJ_C_OBJ_IS_OF_TYPE(T_BASICT, IS_BASIC_TYPE);
		if(IS_NORMAL_TYPE_INSTANCE(obj)) { return 0; }
		dump_titled("Unimplemented type to check", t);
		assert(0=="native_is(): Unimplemented check against builtin type");
	} else {
		if(!IS_NORMAL_TYPE_INSTANCE(obj)) { return 0; }
		return ut_is_ut(NORMAL_TYPE_INSTANCE_TYPE(obj), t);
	}
	dump_titled("Unimplemented type to check", t);
	assert(0=="native_is(): Unimplemented check");
}

void dump(VALUE v) {
	_dump(v, 0);
}

void dump_titled(char *title, VALUE v) {
	printf("=== [ dump %s ] ===\n", title);
	dump(v);
}

// XXX is it safe?
char *obj_to_cstring(VALUE v) {
	char *ret;
	assert(IS_STRING(v));
	ret = NGS_MALLOC_ATOMIC(OBJ_LEN(v) + 1);
	assert(ret);
	memcpy(ret, OBJ_DATA_PTR(v), OBJ_LEN(v));
	ret[OBJ_LEN(v)] = '\0';
	return ret;
}

// TODO: exceptions instead of assert()s?
char **obj_to_cstring_array(VALUE v) {
	size_t i, l;
	char **ret;
	assert(IS_ARRAY(v));
	l = OBJ_LEN(v);
	ret = NGS_MALLOC(sizeof(char *) * (l+1));
	for(i=0; i<l; i++) {
		ret[i] = obj_to_cstring(ARRAY_ITEMS(v)[i]);
	}
	ret[l] = NULL;
	return ret;
}

VALUE _parse_json_kern(json_object *obj) {
	VALUE ret;
	size_t i, len;
	json_type type;
	struct json_object_iterator iter, iter_end;

	type = json_object_get_type(obj);
	switch(type) {
	    case json_type_null:    return (VALUE){.num = V_NULL};
	    case json_type_boolean: return MAKE_BOOL(json_object_get_boolean(obj)); // TODO: check out-of-range
		case json_type_int:     return MAKE_INT(json_object_get_int64(obj));
		case json_type_double:  return make_real(json_object_get_double(obj));
		case json_type_string:  return make_string_of_len(json_object_get_string(obj), json_object_get_string_len(obj));
	    case json_type_array:
			len = json_object_array_length(obj);
			ret = make_array(len);
			for(i=0; i<len; i++) {
				ARRAY_ITEMS(ret)[i] = _parse_json_kern(json_object_array_get_idx(obj, i));
			}
			return ret;
	    case json_type_object:
			ret = make_hash(0);
			iter = json_object_iter_begin(obj);
			iter_end = json_object_iter_end(obj);

			while (!json_object_iter_equal(&iter, &iter_end)) {
				set_hash_key(
					ret,
					make_string(json_object_iter_peek_name(&iter)),
					_parse_json_kern(json_object_iter_peek_value(&iter))
				);
				json_object_iter_next(&iter);
			}
			return ret;
	}
	assert(0 == "Internal error while parsing JSON");
}

METHOD_RESULT parse_json(VALUE s, VALUE *result) {
	json_tokener *tok;
	json_object  *jobj;
	enum json_tokener_error jerr;

	tok = json_tokener_new();
	if(!tok) {
		*result = make_string("Failed to allocate parser");
		return METHOD_EXCEPTION;
	} // TODO: Throw more specific exception

	jobj = json_tokener_parse_ex(tok, OBJ_DATA_PTR(s), OBJ_LEN(s));
	jerr = json_tokener_get_error(tok);

	if(jerr == json_tokener_continue) {
		// See php_json_decode_ex() in php-json-1.3.7/jsonc-1.3.7/json.c
		jobj = json_tokener_parse_ex(tok, "", -1);
		jerr = json_tokener_get_error(tok);
	}

	if(jerr != json_tokener_success) {
		*result = make_string(json_tokener_error_desc(jerr));
		goto error;
	} // TODO: Throw more specific exception

	if(!jobj) {
		*result = make_string("Failed to parse - no resulting object");
		goto error;
	}

	*result = _parse_json_kern(jobj);

	json_tokener_free(tok);
	return METHOD_OK;

error:
	json_tokener_free(tok);
	return METHOD_EXCEPTION;

}


void *ngs_memmem(const void *haystack_start, size_t haystack_len, const void *needle_start, size_t needle_len) {
	const unsigned char *haystack = (const unsigned char *) haystack_start;
	const unsigned char *needle = (const unsigned char *) needle_start;
	const unsigned char *last;

	if (needle_len == 0) return (void *) haystack;
	if (haystack_len < needle_len) return NULL;
	if (needle_len == 1) return memchr(haystack_start, needle[0], haystack_len);

	for (last = haystack_start + haystack_len - needle_len; haystack <= last; haystack++) {
		// printf("start=%p haystack=%p last=%p\n", haystack_start, haystack, last);
		if (haystack[0] == needle[0] && memcmp(haystack, needle, needle_len) == 0)
			return (void *)haystack;
	}

	return NULL;

}

char *ngs_strdup(const char *src) {
	size_t len = strlen(src) + 1;
	char *ret = NGS_MALLOC_ATOMIC(len);
	if (ret == NULL) { return ret; }
	memcpy(ret, src, len);
	return ret;
}

// WIP
// INFO: Backtrace is always needed to throw an exception
//       but it's rarely used so snapshot here whatever is needed
//       so it will be just enough to enable resolving later
// TODO: consider snapshotting local variables of each frame
//       plus arguments info
VALUE make_backtrace(VM *vm, CTX *ctx) {
	VALUE ret, frames, h;
	size_t i;
	(void) vm;
	frames = make_array(ctx->frame_ptr);
	for(i=0; i<ctx->frame_ptr; i++) {
		h = make_hash(4);
		set_hash_key(h, make_string("ip"), MAKE_INT(ctx->frames[i].last_ip));
		set_hash_key(h, make_string("closure"), ctx->frames[i].closure);
		ARRAY_ITEMS(frames)[i] = h;
	}
	ret = make_normal_type_instance(vm->Backtrace);
	set_normal_type_instance_attribute(ret, make_string("frames"), frames);
	return ret;
}
