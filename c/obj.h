// NGS objects
#ifndef OBJ_H
#define OBJ_H
#include <stdint.h>
#include <stddef.h>
typedef union {
	size_t num;
	void * ptr;
} VALUE;

typedef struct object_struct {
	VALUE type;
	VALUE val;
} OBJECT;

// malloc() / NGS_MALLOC() memory is 8 byte aligned
// .....000 - *OBJECT
// .....001 - int number
// .....010 - false
// .....011 - true
// .....100 - null
// .....101 - reserved
// .....110 - reserved
// .....111 - reserved

// object
// .....000 - *TYPE
// .....001 - Lstring

#define META_BITS  (3)
#define META_AND   (7)
#define META_INT   (1)

// TODO: handle situation when n is wider than size_t - META_BITS bits
#define IS_INT(v)    (v.num & META_AND == META_INT)
#define SET_INT(v,n) v.num = (n << META_BITS) | META_INT
#define GET_INT(v)   ((v).num >> META_BITS)
#define SET_OBJ(v,o) v.ptr = o

#define GET_LSTR(out, v) do { OBJECT *o; o=(v).ptr; assert((o->type.num & 7) == 1); out=o->val.ptr; } while(0)
// TODO: check Boehm atomic allocation as we set all the members of *o
#define SET_LSTR(v, lstr_ptr) do { OBJECT *o = NGS_MALLOC(sizeof(o)); SET_OBJECT_TYPE_LSTR(o); o->val.ptr = (lstr_ptr); SET_OBJ((v), o); } while(0)

#define SET_OBJECT_TYPE_LSTR(o) o->type.num = 1

#endif
