#include <assert.h>
#include <execinfo.h>
#include <string.h>
#include "ngs.h"
#include "obj.h"

static void _dump(VALUE v, int level) {
	char **symbols;
	void *symbols_buffer[1];
	VALUE *ptr;
	size_t i;

	if(IS_NULL(v))  { printf("%*s* null\n",    level << 1, ""); goto exit; }
	if(IS_TRUE(v))  { printf("%*s* true\n",    level << 1, ""); goto exit; }
	if(IS_FALSE(v)) { printf("%*s* false\n",   level << 1, ""); goto exit; }
	if(IS_UNDEF(v)) { printf("%*s* undef\n",   level << 1, ""); goto exit; }

	if(IS_INT(v))   { printf("%*s* int %ld\n", level << 1, "", GET_INT(v)); goto exit; }

	if(IS_STRING(v)) {
		printf("%*s* string(len=%zu) %.*s\n", level << 1, "", OBJ_LEN(v), (int) OBJ_LEN(v), (char *)OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_NATIVE_METHOD(v)) {
		symbols_buffer[0] = OBJ_DATA_PTR(v);
		symbols = backtrace_symbols(symbols_buffer, 1);
		printf("%*s* native method %s at %p\n", level << 1, "", symbols[0], OBJ_DATA_PTR(v));
		goto exit;
	}

	if(IS_CLOSURE(v)) {
		printf("%*s* closure ip=%zu locals_including_params=%d req_params=%d opt_params=%d\n", level << 1, "",
			CLOSURE_OBJ_IP(v),
			CLOSURE_OBJ_N_LOCALS(v),
			CLOSURE_OBJ_N_REQ_PAR(v),
			CLOSURE_OBJ_N_OPT_PAR(v)
		);
		for(i=0; i<CLOSURE_OBJ_N_REQ_PAR(v); i++) {
			printf("%*s* required parameter %zu\n", (level+1) << 1, "", i+1);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+0], level+2);
			_dump(CLOSURE_OBJ_PARAMS(v)[i*2+1], level+2);
		}
		if(CLOSURE_OBJ_N_OPT_PAR(v)) {
			printf("%*s* dumping optional parameters is not implemented yet\n", (level+1) << 1, "");
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

	if(IS_NGS_TYPE(v)) {
		printf("%*s* type (name and constructors follow) id=%d\n", level << 1, "", NGS_TYPE_ID(v));
		_dump(NGS_TYPE_NAME(v), level + 1);
		_dump(NGS_TYPE_CONSTRUCTORS(v), level + 1);
		goto exit;
	}

exit:
	return;
}

// TODO: consider allocating power-of-two length
VALUE make_var_len_obj(uintptr_t type, const size_t item_size, const size_t len) {

	VALUE v;
	VAR_LEN_OBJECT *vlo;

	vlo = NGS_MALLOC(sizeof(*vlo));
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

VALUE make_array_with_values(size_t len, VALUE *values) {
	VALUE ret;
	ret = make_array(len);
	memcpy(OBJ_DATA_PTR(ret), values, sizeof(VALUE)*len);
	return ret;
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

VALUE make_closure_obj(size_t ip, LOCAL_VAR_INDEX n_local_vars, LOCAL_VAR_INDEX n_params_required, LOCAL_VAR_INDEX n_params_optional, VALUE *params) {

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
	params_size = (n_params_required*2 + n_params_optional*3) * sizeof(VALUE);
	c->params.params = NGS_MALLOC(params_size);
	assert(c->params.params);
	memcpy(c->params.params, params, params_size);

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

// TODO: make it faster, probably using vector of NATIVE_TYPE_IDs and how to detect them
//       maybe re-work tagged types so the check would be VALUE & TYPE_VAL == TYPE_VAL
// WARNING: t must be IS_NGS_TYPE(t)
// WARNING: only for builtin types!
int obj_is_of_type(VALUE obj, VALUE t) {
	NATIVE_TYPE_ID tid;
	assert(IS_NGS_TYPE(t)); // XXX: Performance hit
	tid = NGS_TYPE_ID(t);
	assert(tid); // XXX: Performance hit
	if(tid == T_ANY) { return 1; }
	OBJ_C_OBJ_IS_OF_TYPE(T_NULL, IS_NULL);
	OBJ_C_OBJ_IS_OF_TYPE(T_BOOL, IS_BOOL);
	OBJ_C_OBJ_IS_OF_TYPE(T_INT, IS_INT);
	OBJ_C_OBJ_IS_OF_TYPE(T_STR, IS_STRING);
	OBJ_C_OBJ_IS_OF_TYPE(T_ARR, IS_ARRAY);
	OBJ_C_OBJ_IS_OF_TYPE(T_TYPE, IS_NGS_TYPE);

	dump_titled("Unimplemented type to check", t);
	assert(0=="native_is(): Unimplemented check against builtin type");
}

void dump(VALUE v) {
	_dump(v, 0);
}

void dump_titled(char *title, VALUE v) {
	printf("=== [ dump %s ] ===\n", title);
	dump(v);
}
