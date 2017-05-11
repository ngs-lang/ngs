#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <errno.h>

#include "ngs.h"

// Poor man's malloc. To be use only between fork() and exec().

int_fast8_t ngs_use_stupid_malloc;
void *ngs_malloc_base = 0;
void *ngs_malloc_next_free = 0;
long ngs_malloc_allocated = 0;

void ngs_malloc_init() {
	long sz = sysconf(_SC_PAGESIZE);
	ngs_malloc_allocated = 1*1024*1024;
	assert(ngs_malloc_allocated % sz == 0);
	ngs_malloc_base = mmap(NULL, ngs_malloc_allocated, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);

	if(ngs_malloc_base == (void *) -1) {
		assert(0 == "MMAP FAILED");
	}
	assert(ngs_malloc_base);
	ngs_malloc_next_free = ngs_malloc_base;
}

void *ngs_malloc(size_t size) {
	void *p;
	if(ngs_use_stupid_malloc) {
		if(size % sizeof(size_t)) {
			size = ((size / sizeof(size_t)) + 1) * sizeof(size_t);
		}
		assert(ngs_malloc_next_free - ngs_malloc_base + sizeof(size_t) + size < ngs_malloc_allocated);
		*(size_t *)ngs_malloc_next_free = size;
		ngs_malloc_next_free += sizeof(size_t);
		p = ngs_malloc_next_free;
		ngs_malloc_next_free += size;
		memset(p, 0, size);
		return p;
	} else {
		return GC_MALLOC(size);
	}
}

void *ngs_malloc_atomic(size_t size) {
	if(ngs_use_stupid_malloc) {
		return ngs_malloc(size);
	} else {
		return GC_MALLOC_ATOMIC(size);
	}

}
void *ngs_realloc(void *ptr, size_t size) {
	int i;
	if(ngs_use_stupid_malloc) {
		if(ptr == NULL) {
			return ngs_malloc(size);
		}
		if(ptr >= ngs_malloc_base && ptr < ngs_malloc_base + ngs_malloc_allocated) {
			void *p = ngs_malloc(size);
			memcpy(p, ptr, ((size_t *)ptr)[-1]);
			return p;
		}
		assert(0 == "Realloc not implemented for NGS_STD_MALLOC_AFTER_FORK");
	} else {
		return GC_REALLOC(ptr, size);
	}
}
