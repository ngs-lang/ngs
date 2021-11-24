#include <assert.h>
#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif
#include <inttypes.h>
#include <string.h>

#include <json-c/json.h>

#include "ngs.h"
#include "obj.h"
#include "vm.h"

static void _dump(FILE *f, VALUE v, int level) {
	char **symbols;
	void *symbols_buffer[1];
	VALUE *ptr;
	size_t i;
	HASH_OBJECT_ENTRY *e;
	HASH_OBJECT_ENTRY **buckets;

	if(IS_NULL(v))  { fprintf(f, "%*s* null\n",    level << 1, ""); goto exit; }
	if(IS_TRUE(v))  { fprintf(f, "%*s* true\n",    level << 1, ""); goto exit; }
	if(IS_FALSE(v)) { fprintf(f, "%*s* false\n",   level << 1, ""); goto exit; }
	if(IS_UNDEF(v)) { fprintf(f, "%*s* undef\n",   level << 1, ""); goto exit; }
	if(IS_KWARGS_MARKER(v)){ fprintf(f, "%*s* kwargs marker\n",   level << 1, ""); goto exit; }

	if(IS_INT(v))   { fprintf(f, "%*s* int %" VALUE_NUM_FMT "\n", level << 1, "", GET_INT(v)); goto exit; }
	if(IS_REAL(v))  { fprintf(f, "%*s* real %g\n", level << 1, "", REAL_OBJECT_VAL(v)); goto exit; }

	if(IS_STRING(v)) {
		// TODO: properly handle
		//       1. non-printable characters
		//       2. zero character
		fprintf(f, "%*s* string(len=%zu) %.*s\n", level << 1, "", OBJ_LEN(v), (int) OBJ_LEN(v), (char *)OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_NATIVE_METHOD(v)) {
		symbols_buffer[0] = OBJ_DATA_PTR(v);
#ifdef HAVE_EXECINFO_H
		symbols = backtrace_symbols(symbols_buffer, 1);
#else
		symbols = NGS_MALLOC(sizeof(char *));
		symbols[0] = ngs_strdup("(name unavailable due to missing execinfo.h at build time)");

#endif
		fprintf(f, "%*s* native method %s at %p req_params=%d\n", level << 1, "", symbols[0], OBJ_DATA_PTR(v), NATIVE_METHOD_OBJ_N_REQ_PAR(v));
		for(i=0; i<NATIVE_METHOD_OBJ_N_REQ_PAR(v); i++) {
			fprintf(f, "%*s* required parameter %zu\n", (level+1) << 1, "", i+1);
			_dump(f, NATIVE_METHOD_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(f, NATIVE_METHOD_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		goto exit;
	}

	if(IS_CLOSURE(v)) {
		fprintf(f, "%*s* ip=%zu locals_including_params=%d req_params=%d opt_params=%d n_uplevels=%d params_flags=%d\n", level << 1, "",
			CLOSURE_OBJ_IP(v),
			CLOSURE_OBJ_N_LOCALS(v),
			CLOSURE_OBJ_N_REQ_PAR(v),
			CLOSURE_OBJ_N_OPT_PAR(v),
			CLOSURE_OBJ_N_UPLEVELS(v),
			CLOSURE_OBJ_PARAMS_FLAGS(v)
		);
		fprintf(f, "%*s* closure attributes\n", (level+1) << 1, "");
		_dump(f, OBJ_ATTRS(v), level+2);
		for(i=0; i<CLOSURE_OBJ_N_REQ_PAR(v); i++) {
			fprintf(f, "%*s* required parameter %zu (name and type follow)\n", (level+1) << 1, "", i+1);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		for(i=0; i<CLOSURE_OBJ_N_OPT_PAR(v); i++) {
			fprintf(f, "%*s* optional parameter %zu (name, type and default value follow)\n", (level+1) << 1, "", i+1);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[CLOSURE_OBJ_N_REQ_PAR(v)*2 + i*3 + 0], level+2);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[CLOSURE_OBJ_N_REQ_PAR(v)*2 + i*3 + 1], level+2);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[CLOSURE_OBJ_N_REQ_PAR(v)*2 + i*3 + 2], level+2);
		}
		i = CLOSURE_OBJ_N_REQ_PAR(v)*2 + CLOSURE_OBJ_N_OPT_PAR(v)*3;
		if(CLOSURE_OBJ_PARAMS_FLAGS(v) & PARAMS_FLAG_ARR_SPLAT) {
			fprintf(f, "%*s* array splat parameter\n", (level+1) << 1, "");
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i+0], level+2);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i+1], level+2);
			i+=2;
		}
		if(CLOSURE_OBJ_PARAMS_FLAGS(v) & PARAMS_FLAG_HASH_SPLAT) {
			fprintf(f, "%*s* hash splat parameter\n", (level+1) << 1, "");
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i+0], level+2);
			_dump(f, CLOSURE_OBJ_PARAMS(v)[i+1], level+2);
			i+=2;
		}
		goto exit;
	}

	if(IS_ARRAY(v)) {
		fprintf(f, "%*s* array of length %zu\n", level << 1, "", OBJ_LEN(v));
		for(i=0, ptr=(VALUE *)OBJ_DATA_PTR(v); i<OBJ_LEN(v); i++, ptr++) {
			_dump(f, *ptr, level+1);
		}
		goto exit;
	}

	if(IS_MULMETHOD(v)) {
		fprintf(f, "%*s* multimethod\n", level << 1, "");
		fprintf(f, "%*s* methods\n", (level+1) << 1, "");
		_dump(f, MULTIMETHOD_METHODS(v), level+2);
		goto exit;
	}

	if(IS_HASH(v)) {
		fprintf(f, "%*s* hash with total of %zu items in %zu buckets at %p\n", level << 1, "", OBJ_LEN(v), HASH_BUCKETS_N(v), OBJ_DATA_PTR(v));
		buckets = OBJ_DATA_PTR(v);
		for(i=0; i<HASH_BUCKETS_N(v); i++) {
			if(!buckets[i]) { continue; }
			fprintf(f, "%*s* bucket # %zu\n", (level+1) << 1, "", i);
			for(e=buckets[i]; e; e=e->bucket_next) {
				fprintf(f, "%*s* item at %p with hash() of %u insertion_order_prev=%p insertion_order_next=%p \n", (level+2) << 1, "", (void *)e, e->hash, (void *)e->insertion_order_prev, (void *)e->insertion_order_next);
				fprintf(f, "%*s* key\n", (level+3) << 1, "");
				_dump(f, e->key, level+4);
				fprintf(f, "%*s* value\n", (level+3) << 1, "");
				_dump(f, e->val, level+4);
			}
		}
		goto exit;
	}

	if(IS_NGS_TYPE(v)) {
		fprintf(f, "%*s* type (name and optionally constructors and parents follow) id=%" PRIdPTR " ptr=%p\n", level << 1, "", NGS_TYPE_ID(v), IS_NORMAL_TYPE(v) ? v.ptr : 0);
		_dump(f, NGS_TYPE_NAME(v), level + 1);
		if(level < 3) {
			_dump(f, NGS_TYPE_FIELDS(v), level + 1);
			_dump(f, NGS_TYPE_CONSTRUCTORS(v), level + 1);
			_dump(f, NGS_TYPE_PARENTS(v), level + 1);
		}
		goto exit;
	}

	if(IS_CLIB(v)) {
		fprintf(f, "%*s* C library (name follows) ptr=%p\n", level << 1, "", OBJ_DATA_PTR(v));
		_dump(f, CLIB_OBJECT_NAME(v), level + 1);
		goto exit;
	}

	if(IS_CSYM(v)) {
		fprintf(f, "%*s* C symbol (name and library follow) ptr=%p\n", level << 1, "", OBJ_DATA_PTR(v));
		_dump(f, CSYM_OBJECT_NAME(v), level + 1);
		_dump(f, CSYM_OBJECT_LIB(v), level + 1);
		goto exit;
	}

	if(IS_NORMAL_TYPE_CONSTRUCTOR(v)) {
		fprintf(f, "%*s* user type constructor (type optionally follows)\n", level << 1, "");
		if(level < 3) {
			_dump(f, OBJ_DATA(v), level + 1);
		}
		goto exit;
	}

	if(IS_NORMAL_TYPE_INSTANCE(v)) {
		fprintf(f, "%*s* user type instance (type and fields optionally follow)\n", level << 1, "");
		// level < 4 so that uncaught exception MethodNotFound could display the type of the arguments
		if(level < 4) {
			HASH_OBJECT_ENTRY *e;
			VALUE fields = NGS_TYPE_FIELDS(NORMAL_TYPE_INSTANCE_TYPE(v));
			assert(IS_HASH(fields));
			_dump(f, NORMAL_TYPE_INSTANCE_TYPE(v), level + 1);
			for(e=HASH_HEAD(fields); e; e=e->insertion_order_next) {
				if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(v)) <= (size_t)GET_INT(e->val)) {
					continue;
				}
				if(!IS_UNDEF(ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(v))[GET_INT(e->val)])) {
					fprintf(f, "%*s* key:\n", (level+1) << 1, "");
					_dump(f, e->key, level + 2);
					fprintf(f, "%*s* value:\n", (level+1) << 1, "");
					_dump(f, ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(v))[GET_INT(e->val)], level + 2);
				}
			}
		}
		goto exit;
	}

	if(IS_PTHREAD(v)) {
		fprintf(f, "%*s* pthread_t at %p\n", level << 1, "", &GET_PTHREAD(v));
		goto exit;
	}

	if(IS_PTHREADATTR(v)) {
		fprintf(f, "%*s* pthread_attr_t at %p\n", level << 1, "", &GET_PTHREADATTR(v));
		goto exit;
	}

	if(IS_PTHREADMUTEX(v)) {
		fprintf(f, "%*s* pthread_mutex_t at %p\n", level << 1, "", &GET_PTHREADMUTEX(v));
		goto exit;
	}

	if(IS_FFI_TYPE(v)) {
		fprintf(f, "%*s* ffi_type at %p\n", level << 1, "", &GET_FFI_TYPE(v));
		goto exit;
	}

	if(IS_FFI_CIF(v)) {
		fprintf(f, "%*s* ffi_cif at %p\n", level << 1, "", &GET_FFI_CIF(v));
		goto exit;
	}

	fprintf(f, "%*s* (dump not implemented for the object at %p)\n", level << 1, "", OBJ_DATA_PTR(v));

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

VALUE _make_multimethod() {
	VALUE ret;
	MULTIMETHOD_OBJECT *multimethod;
	multimethod = NGS_MALLOC(sizeof(*multimethod));
	assert(multimethod);
	SET_OBJ(ret, multimethod);
	OBJ_TYPE_NUM(ret) = T_MULMETHOD;
	return ret;
}

VALUE make_multimethod() {
	VALUE ret = _make_multimethod();
	MULTIMETHOD_METHODS(ret) = make_array(0);
	return ret;
}

VALUE make_multimethod_with_value(const VALUE value) {
	VALUE ret = _make_multimethod();
	MULTIMETHOD_METHODS(ret) = make_array(1);
	ARRAY_ITEMS(MULTIMETHOD_METHODS(ret))[0] = value;
	return ret;
}

VALUE make_multimethod_from_array(const VALUE arr) {
	VALUE ret = _make_multimethod();
	MULTIMETHOD_METHODS(ret) = arr;
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
		assert(OBJ_DATA_PTR(ret));
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

VALUE make_namespace(size_t start_buckets) {
	VALUE ret;
	ret = make_hash(start_buckets);
	OBJ_TYPE_NUM(ret) = T_NAMESPACE;
	return ret;
}

VALUE make_normal_type(VALUE name) {
	VALUE ret;
	NGS_TYPE *t;
	t = NGS_MALLOC(sizeof(*t));
	assert(t);

	SET_OBJ(ret, t);
	OBJ_TYPE_NUM(ret) = T_TYPE;

	VALUE ctr = make_normal_type_constructor(ret);

	NGS_TYPE_NAME(ret) = name;
	NGS_TYPE_FIELDS(ret) = make_hash(8); // Hash: name->index
	NGS_TYPE_CONSTRUCTORS(ret) = make_multimethod_with_value(ctr);
	NGS_TYPE_PARENTS(ret) = make_array(0);
	NGS_TYPE_USER(ret) = make_hash(0);
	OBJ_ATTRS(ret) = make_hash(2);

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
}

METHOD_RESULT get_normal_type_instace_field(VALUE obj, VALUE field, VALUE *result) {
	VALUE ut;
	HASH_OBJECT_ENTRY *e;
	size_t n;
	ut = NORMAL_TYPE_INSTANCE_TYPE(obj);
	e = get_hash_key(NGS_TYPE_FIELDS(ut), field);
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
}

void set_normal_type_instance_field(VALUE obj, VALUE field, VALUE v) {
	VALUE ut;
	HASH_OBJECT_ENTRY *e;
	size_t n;
	ut = NORMAL_TYPE_INSTANCE_TYPE(obj);
	e = get_hash_key(NGS_TYPE_FIELDS(ut), field);
	if(e) {
		n = GET_INT(e->val);
	} else {
		n = OBJ_LEN(NGS_TYPE_FIELDS(ut));
		set_hash_key(NGS_TYPE_FIELDS(ut), field, MAKE_INT(n));
	}
	// TODO: more optimized
	while(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(obj)) < n) {
		array_push(NORMAL_TYPE_INSTANCE_FIELDS(obj), MAKE_UNDEF);
	}
	if(OBJ_LEN(NORMAL_TYPE_INSTANCE_FIELDS(obj)) == n) {
		array_push(NORMAL_TYPE_INSTANCE_FIELDS(obj), v);
		return;
	}
	ARRAY_ITEMS(NORMAL_TYPE_INSTANCE_FIELDS(obj))[n] = v;
}

void add_type_inheritance(VALUE type, VALUE parent_type) {
	assert(IS_NGS_TYPE(type));
	assert(IS_NGS_TYPE(parent_type));
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
	if(IS_NULL(a) && IS_NULL(b)) {
		return 1;
	}
	if(IS_BOOL(a) && IS_BOOL(b)) {
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
		// Warning: only using lower 32 significant bits out of 60 significant bits on x86_64.
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
	uint32_t n;
	if(HASH_BUCKETS_N(h) == 0) {
		return 0;
	}
	n = hash(k) % HASH_BUCKETS_N(h);
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

// TODO: do it without rehashing again, assuming (currently true) it's the same hashing function
void update_hash(VALUE dst, VALUE src) {
	assert(IS_HASH(dst));
	assert(IS_HASH(src));
	HASH_OBJECT_ENTRY *e;
	for(e=HASH_HEAD(src); e; e=e->insertion_order_next) {
		set_hash_key(dst, e->key, e->val);
	}
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

VALUE make_regexp() {
	VALUE v;
	REGEXP_OBJECT *regexp;
	regexp = NGS_MALLOC(sizeof(*regexp));
	regexp->base.type.num = T_REGEXP;
	SET_OBJ(v, regexp);
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

void push_multimethod_method(VALUE multimethod, const VALUE method) {
	assert(IS_MULMETHOD(multimethod));
	array_push(MULTIMETHOD_METHODS(multimethod), method);
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

VALUE make_closure_obj(size_t ip, LOCAL_VAR_INDEX n_local_vars, LOCAL_VAR_INDEX n_params_required, LOCAL_VAR_INDEX n_params_optional, UPVAR_INDEX n_uplevels, int params_flags, VALUE *params, VALUE *locals) {

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

	c->params.locals = NGS_MALLOC(n_local_vars * sizeof(c->params.locals[0]));
	assert(c->params.locals);
	memcpy(c->params.locals, locals, n_local_vars * sizeof(c->params.locals[0]));

	SET_OBJ(v, c);
	OBJ_ATTRS(v) = make_hash(2);

	return v;
}

VALUE join_strings(int argc, VALUE *argv) {
	size_t len;
	int i;
	VALUE ret;
	char *dst;

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

VALUE value_type(VM *vm, VALUE val) {

	VALUE *p;

	// Tagged value
	if(IS_INT(val)) {
		return vm->Int;
	}

	// Immediate (kind of tagged) values
	if(val.num & TAG_AND) {
		int n = val.num >> TAG_BITS;
		assert(n <= MAX_VALUE_TAG_VALUE);
		p = vm->type_by_value_tag[n];
		if(!p) {
			if(IS_UNDEF(val)) {
				assert(0 == "value_type() - val is undefined");
			}
			fprintf(stderr, "N %d\n", n);
			assert(0 == "value_type() - missing vm->type_by_value_tag");
		}
		return *p;
	}

	// Built-in type
	int otn = OBJ_TYPE_NUM(val);
	if(otn & TAG_BITS) {
		if((otn & T_OBJ) == T_OBJ) {
			p = vm->type_by_t_obj_type_id[otn >> T_OBJ_TYPE_SHIFT_BITS];
			if(!p) {
				fprintf(stderr, "OTN %d OTN with shift %d\n", otn, otn >> T_OBJ_TYPE_SHIFT_BITS);
				assert(0 == "value_type() - unhandled built-in type, not present in vm->type_by_t_obj_type_id");
			}
			return *p;
		}
		fprintf(stderr, "OTN %d\n", otn);
		// dump_titled("val", val);
		assert(0 == "value_type() - unhandled built-in type");
	}

	// Normal type (the only option that is left)
	return NORMAL_TYPE_INSTANCE_TYPE(val);
};

// TODO: T_ANY, maybe some other special cases
// TODO: Fix for ut_child being internal type
int type_is_type(VALUE ut_child, VALUE ut_parent) {
	if(ut_child.ptr == ut_parent.ptr) { return 1; }
	size_t len, i;
	for(i=0, len=OBJ_LEN(NGS_TYPE_PARENTS(ut_child)); i<len; i++) {
		if(type_is_type(ARRAY_ITEMS(NGS_TYPE_PARENTS(ut_child))[i], ut_parent)) {
			return 1;
		}
	}
	return 0;
}

#define OBJ_C_OBJ_IS_OF_TYPE(type, check) if(tid == type) { return check(obj); }

// TODO: make it faster, probably using vector of NATIVE_TYPE_IDs and how to detect them
//       maybe re-work tagged types so the check would be VALUE & TYPE_VAL == TYPE_VAL
// TODO: profiling before optimizations
int obj_is_of_type(VM *vm, VALUE obj, VALUE t) {
	VALUE_NUM tid;
	assert(IS_NGS_TYPE(t));
	tid = NGS_TYPE_ID(t);
	if(tid) {
		if(tid == T_ANY) { return 1; }
		// printf("TID %d\n", tid);
		if((tid & 0xff) == T_OBJ) {
			if(!IS_OBJ(obj)) {
				return 0;
			}
			return type_is_type(value_type(vm, obj), t);
		}
		OBJ_C_OBJ_IS_OF_TYPE(T_NULL, IS_NULL);
		OBJ_C_OBJ_IS_OF_TYPE(T_BOOL, IS_BOOL);
		OBJ_C_OBJ_IS_OF_TYPE(T_INT, IS_INT);
		OBJ_C_OBJ_IS_OF_TYPE(T_NORMTI, IS_NORMAL_TYPE_INSTANCE);
		OBJ_C_OBJ_IS_OF_TYPE(T_BASICTI, IS_BASIC_TYPE_INSTANCE);
		if(tid == T_FUN) {
			return IS_MULMETHOD(obj) || IS_NATIVE_METHOD(obj) || IS_CLOSURE(obj) || IS_NGS_TYPE(obj);
		}
		OBJ_C_OBJ_IS_OF_TYPE(T_NORMT, IS_NORMAL_TYPE);
		OBJ_C_OBJ_IS_OF_TYPE(T_BASICT, IS_BASIC_TYPE);
	}
	return type_is_type(value_type(vm, obj), t);
}

void dump(FILE *f, VALUE v) {
	_dump(f, v, 0);
}

void dump_titled(FILE *f, char *title, VALUE v) {
	fprintf(f, "=== [ dump %s ] ===\n", title);
	dump(f, v);
}

// XXX is it safe?
// XXX not binary safe, check and fix all call sites
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

VALUE _decode_json_kern(json_object *obj) {
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
				ARRAY_ITEMS(ret)[i] = _decode_json_kern(json_object_array_get_idx(obj, i));
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
					_decode_json_kern(json_object_iter_peek_value(&iter))
				);
				json_object_iter_next(&iter);
			}
			return ret;
	}
	assert(0 == "Internal error while parsing JSON");
}

METHOD_RESULT decode_json(VM *vm, VALUE s, VALUE *result) {

	// https://github.com/json-c/json-c/blob/master/json_tokener.h

	json_tokener *tok;
	json_object  *jobj;
	enum json_tokener_error jerr;

	tok = json_tokener_new();
	if(!tok) {
		*result = make_string("Failed to allocate JSON parser");
		return METHOD_EXCEPTION;
	}

	jobj = json_tokener_parse_ex(tok, OBJ_DATA_PTR(s), OBJ_LEN(s));
	jerr = json_tokener_get_error(tok);

	if(jerr == json_tokener_continue) {
		// See php_json_decode_ex() in php-json-1.3.7/jsonc-1.3.7/json.c
		jobj = json_tokener_parse_ex(tok, "", -1);
		jerr = json_tokener_get_error(tok);
	}

	if(jerr != json_tokener_success) {
		*result = make_normal_type_instance(vm->JsonDecodeFail);
		set_normal_type_instance_field(*result, make_string("error"), make_string(json_tokener_error_desc(jerr)));
		set_normal_type_instance_field(*result, make_string("value"), s);
		// XXX: Leaking abstraction tok->char_offset. Unfortunately I did not see any alternative.
		//      2021-11-24 tok->... is now deprecated
		//      tok->char_offset will be json_tokener_get_parse_end(tok)
		set_normal_type_instance_field(*result, make_string("position"), MAKE_INT(tok->char_offset));
		goto error;
	}

	*result = _decode_json_kern(jobj);

	json_tokener_free(tok);
	return METHOD_OK;

error:
	json_tokener_free(tok);
	return METHOD_EXCEPTION;

}

// TODO: include the object and/or it's type in the exception info
// TODO: handle libjson errors
// XXX: free the memory on fails
json_object *_encode_json_kern(VALUE obj, VALUE *result) {
	unsigned int i;
	if(IS_NULL(obj)) { return NULL; }
	if(IS_STRING(obj)) { return json_object_new_string_len(OBJ_DATA_PTR(obj), OBJ_LEN(obj)); }
	if(IS_INT(obj)) { return json_object_new_int64(GET_INT(obj)); }
	if(IS_BOOL(obj)) { return json_object_new_boolean(IS_TRUE(obj)); }
	if(IS_REAL(obj)) { return json_object_new_double(GET_REAL(obj)); }
	if(IS_ARRAY(obj)) {
		json_object *t, *arr = json_object_new_array();
		// TODO: replace unsigned int with something more appropriate
		for(i=0; i<OBJ_LEN(obj); i++) {
			t = _encode_json_kern(ARRAY_ITEMS(obj)[i], result);
			if(!IS_UNDEF(*result)) return NULL; // Exception occurred
			json_object_array_add(arr, t);
		}
		return arr;
	}
	if(IS_HASH(obj)) {
		json_object *v, *hash = json_object_new_object();
		HASH_OBJECT_ENTRY *e;
		for(e=HASH_HEAD(obj); e; e=e->insertion_order_next) {
			if(!IS_STRING(e->key)) {
				*result = make_string("Hash keys must be strings");
			}
			v = _encode_json_kern(e->val, result);
			if(!IS_UNDEF(*result)) return NULL; // Exception occurred
			json_object_object_add(hash, obj_to_cstring(e->key), v);
		}
		return hash;
	}
	*result = make_string("Don't know how to encode to JSON given object");
	return NULL;
}

// TODO: consider checking that json_object_put() returns 1
METHOD_RESULT encode_json(VALUE obj, VALUE *result) {
	json_object * jobj;
	*result = MAKE_UNDEF;
	jobj = _encode_json_kern(obj, result);
	if(!IS_UNDEF(*result)) {
		return METHOD_EXCEPTION;
	}
	*result = make_string(json_object_to_json_string(jobj));
	json_object_put(jobj);
	return METHOD_OK;
}

void *ngs_memmem(const void *haystack_start, size_t haystack_len, const void *needle_start, size_t needle_len) {
	const unsigned char *haystack = (const unsigned char *) haystack_start;
	const unsigned char *needle = (const unsigned char *) needle_start;
	const unsigned char *last;

	if (needle_len == 0) return (void *) haystack;
	if (haystack_len < needle_len) return NULL;
	if (needle_len == 1) return memchr(haystack_start, needle[0], haystack_len);

	for (last = (unsigned char *)haystack_start + haystack_len - needle_len; haystack <= last; haystack++) {
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
char *ngs_strcat(const char *s1, const char *s2) {
	size_t len = strlen(s1) + strlen(s2) + 1;
	char *ret = NGS_MALLOC_ATOMIC(len);
	if (ret == NULL) { return ret; }
	memcpy(ret, s1, strlen(s1));
	memcpy(ret + strlen(s1), s2, strlen(s2)+1);
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
	set_normal_type_instance_field(ret, make_string("frames"), frames);
	return ret;
}

VALUE make_pthread() {
	VALUE v;
	PTHREAD_OBJECT *p;
	// TODO: NGS_MALLOC_ATOMIC maybe?
	p = NGS_MALLOC(sizeof(*p));
	p->base.type.num = T_PTHREAD;
	SET_OBJ(v, p);
	return v;
}

VALUE make_pthread_attr() {
	VALUE v;
	PTHREADATTR_OBJECT *pa;
	pa = NGS_MALLOC(sizeof(*pa));
	pa->base.type.num = T_PTHREADATTR;
	SET_OBJ(v, pa);
	return v;
}

VALUE make_pthread_mutex() {
	VALUE v;
	PTHREADMUTEX_OBJECT *pm;
	// TODO: NGS_MALLOC_ATOMIC maybe?
	pm = NGS_MALLOC(sizeof(*pm));
	pm->base.type.num = T_PTHREADMUTEX;
	SET_OBJ(v, pm);
	return v;
}

VALUE make_pthread_mutexattr() {
	VALUE v;
	PTHREADMUTEXATTR_OBJECT *pma;
	pma = NGS_MALLOC(sizeof(*pma));
	pma->base.type.num = T_PTHREADMUTEXATTR;
	SET_OBJ(v, pma);
	return v;
}

VALUE make_pthread_cond() {
	VALUE v;
	PTHREADCOND_OBJECT *pc;
	// TODO: NGS_MALLOC_ATOMIC maybe?
	pc = NGS_MALLOC(sizeof(*pc));
	pc->base.type.num = T_PTHREADCOND;
	SET_OBJ(v, pc);
	return v;
}

VALUE make_ffi_type(ffi_type *t) {
	VALUE v;
	FFI_TYPE_OBJECT *tmp;
	// TODO: NGS_MALLOC_ATOMIC maybe?
	tmp = NGS_MALLOC(sizeof(*tmp));
	tmp->base.type.num = T_FFI_TYPE;
	SET_OBJ(v, tmp);
	GET_FFI_TYPE(v) = t;
	return v;
}

VALUE make_ffi_cif() {
	VALUE v;
	FFI_CIF_OBJECT *tmp;
	// TODO: NGS_MALLOC_ATOMIC maybe?
	tmp = NGS_MALLOC(sizeof(*tmp));
	tmp->base.type.num = T_FFI_CIF;
	SET_OBJ(v, tmp);
	return v;
}

VALUE make_DIR() {
	VALUE v;
	DIR_OBJECT *tmp = NGS_MALLOC(sizeof(*tmp));
	tmp->base.type.num = T_DIR;
	SET_OBJ(v, tmp);
	DIR_OBJECT_DIR(v) = NULL;
	return v;
}

VALUE resolve_instruction_pointer(VM *vm, IP ip) {
	// Find region
	// TODO: Something faster then sequential scan
	VALUE ret;
	VM_REGION *region;
	source_tracking_entry *ste;
	int found = 0;
	for(region=vm->regions; region < vm->regions + vm->regions_len; region++) {
		if((ip >= region->start_ip) && (ip < region->start_ip + region->len)) {
			found = 1;
			break;
		}
	}
	if(!found) {
		return MAKE_NULL;
	}
	found = 0;
	if(!region->source_tracking_entries) {
		// The section is optional
		return MAKE_NULL;
	}
	ip = ip - region->start_ip;
	for(ste = region->source_tracking_entries + region->source_tracking_entries_count - 1; ste >= region->source_tracking_entries; ste--) {
		if(ip >= ste->ip) {
			found = 1;
			break;
		}
	}
	if(!found) {
		return MAKE_NULL;
	}
	ret = make_hash(8);
	set_hash_key(ret, make_string("ip"), MAKE_INT(ip));
	set_hash_key(ret, make_string("file"), make_string(region->files_names[ste->source_file_name_idx]));
	set_hash_key(ret, make_string("first_line"), MAKE_INT(ste->source_location[0]));
	set_hash_key(ret, make_string("first_column"), MAKE_INT(ste->source_location[1]));
	set_hash_key(ret, make_string("last_line"), MAKE_INT(ste->source_location[2]));
	set_hash_key(ret, make_string("last_column"), MAKE_INT(ste->source_location[3]));
	return ret;

}
