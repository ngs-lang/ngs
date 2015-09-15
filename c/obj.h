// NGS objects
#ifndef OBJ_H
#define OBJ_H
#include <stdint.h>
#include <stddef.h>
typedef union {
	size_t num;
	void * ptr;
} VALUE;

// malloc() memory is 8 byte aligned
// ....000 - int number
// ....001 - null
// ....010 - false
// ....011 - true
// ....100 - *Lstr
// ....111 - *object

#define META_BITS  (3)
#define META_AND   (7)
#define META_INT   (0)
#define META_LSTR  (4)

// TODO: handle situation when n is wider than size_t - META_BITS bits
#define IS_INT(v)    (v.num & META_AND == META_INT)
#define SET_INT(v,n) v.num = (n << META_BITS) | META_INT
#define GET_INT(v)   ((v).num >> META_BITS)

#define SET_LSTR(v,lstr) (v).ptr = lstr; (v).num |= META_LSTR
#define GET_LSTR(v)      (char *)((v).num & (SIZE_MAX ^ META_AND))

#endif
