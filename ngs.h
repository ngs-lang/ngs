#ifndef NGS_H
#define NGS_H

#ifdef __APPLE__
#define NGS_STUPID_MALLOC_AFTER_FORK
#endif

#include <stdint.h>
#include <stdio.h>
#include <pthread.h>

#include "version.h"

// GC - start
// http://www.hboehm.info/gc/scale.html
#define GC_THREADS
#define PARALLEL_MARK
#define THREAD_LOCAL_ALLOC

#include <gc.h>
#define NGS_GC_THR_INIT() GC_thr_init()

#ifdef NGS_STUPID_MALLOC_AFTER_FORK
extern int_fast8_t ngs_use_stupid_malloc;
void ngs_malloc_init();
void *ngs_malloc(size_t size);
void *ngs_malloc_atomic(size_t size);
void *ngs_realloc(void *ptr, size_t size);
#  define NGS_GC_INIT() GC_set_handle_fork(1); GC_INIT(); ngs_use_stupid_malloc = 0;
#  define NGS_MALLOC(n) ngs_malloc(n)
#  define NGS_MALLOC_ATOMIC(n) ngs_malloc_atomic(n)
// There is a problem to know which chunks were allocated with gc malloc and which with standard malloc
#  define NGS_REALLOC(p, n) ngs_realloc(p, n)
#  define NGS_GCOLLECT() GC_gcollect()
#  define NGS_SIZE(p) GC_size(p)
#  define NGS_NOTIFY_MALLOC_ABOUT_FORK() ngs_use_stupid_malloc = 1; ngs_malloc_init();
#else
#  define NGS_GC_INIT() GC_set_handle_fork(1); GC_INIT()
#  define NGS_MALLOC(n) GC_MALLOC(n)
#  define NGS_MALLOC_ATOMIC(n) GC_MALLOC_ATOMIC(n)
#  define NGS_REALLOC(p, n) GC_REALLOC(p, n)
#  define NGS_GCOLLECT() ngs_gc_collect()
#  define NGS_SIZE(p) GC_size(p)
#  define NGS_NOTIFY_MALLOC_ABOUT_FORK() (void)0
#endif


#define NGS_MALLOC_OBJ(dst) dst = NGS_MALLOC(sizeof(*dst)); assert(dst);

#define YY_MALLOC(ctx, size)        NGS_MALLOC(size)
#define YY_REALLOC(ctx, ptr, size)  NGS_REALLOC(ptr, size)
#define YY_FREE(ctx, ptr)           (void)(ptr)

// GC - end

typedef struct VM VM;

// XXX: don't do NGS_REALLOC all the time. Maybe re-use some
//      other code here instead of reinventing the wheel
#define ENSURE_ARRAY_ROOM(dst, allocated, len, min_len) { \
	assert(min_len); \
	if(!allocated) { \
		allocated = min_len; \
		dst = NGS_MALLOC(sizeof(*dst) * allocated); \
	} \
	while(allocated < len) { \
		allocated *= 2; \
		dst = NGS_REALLOC(dst, sizeof(*dst) * allocated); \
	} \
	assert(dst); \
}

#define PUSH_ARRAY_ELT(dst, len, elt) { \
	(dst)[(len)++] = elt; \
}

#include "debug.h"

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

typedef enum {
	METHOD_OK,
	METHOD_ARGS_MISMATCH,
	METHOD_IMPL_MISSING,
	METHOD_EXCEPTION,
} METHOD_RESULT;

extern pthread_key_t thread_local_key;
#endif
