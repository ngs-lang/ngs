// NGS objects
#ifndef OBJ_H
#define OBJ_H
#include <stdint.h>
typedef union {
	int32_t num;
	void * ptr;
} obj;

// malloc() memory is 8 byte aligned
// ....000 - int number
// ....001 - null
// ....010 - false
// ....011 - true
// ....100 - empty string
// ....101 - 
// ....110 - object

#define META_BITS (3)
#define META_AND  (7)

#define IS_INT(obj)    (obj.num & META_AND == 0)
// TODO: handle situation when n is wider than int32_t - META_BITS bits
#define SET_INT(obj,n) obj.num = n << META_BITS
#define GET_INT(obj)   (obj.num >> META_BITS)

#define MK_OBJ         ((obj *) malloc(sizeof(obj)))

#endif
