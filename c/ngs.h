#ifndef NGS_H
#define NGS_H

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// GC - start
// http://www.hboehm.info/gc/scale.html
#define GC_THREADS
#define PARALLEL_MARK
#define THREAD_LOCAL_ALLOC

#define GC_REDIRECT_TO_LOCAL

#include <gc.h>
#define NGS_GC_INIT() GC_INIT()
#define NGS_GC_THR_INIT() GC_thr_init()
#define NGS_MALLOC(n) GC_MALLOC(n)
#define NGS_REALLOC(p, n) GC_REALLOC(p, n)
#define NGS_MALLOC_UNCOLLECTALE(n) GC_MALLOC_UNCOLLECTABLE(n)
#define NGS_MALLOC_ATOMIC(n) GC_MALLOC_ATOMIC(n)
#define NGS_GCOLLECT() GC_gcollect()
// GC - end

// DEBUG facilities - start
// TODO: maybe do compile time decisions per debug flag
//       instead of one global yes/no and then runtime per flag decisions
extern uint32_t debug_flags;

#define DEBUG_FLAG_BYTECODE (1 << 0)
#define DEBUG_FLAG_PARSER   (1 << 1)
#define DEBUG_FLAG_COMPILER (1 << 2)
#define DEBUG_FLAG_VM_API   (1 << 3)
#define DEBUG_FLAG_VM_RUN   (1 << 4)

#define PRINTF_DEBUG(flag, format, ...) if(debug_flags & (flag)) printf("[DEBUG] " format, __VA_ARGS__)
#define IF_DEBUG(what, code) if(debug_flags & (DEBUG_FLAG_ ## what)) { code }

#define DEBUG_PARSER(...)   PRINTF_DEBUG(DEBUG_FLAG_PARSER,   __VA_ARGS__)
#define DEBUG_COMPILER(...) PRINTF_DEBUG(DEBUG_FLAG_COMPILER, __VA_ARGS__)
#define DEBUG_VM_API(...)   PRINTF_DEBUG(DEBUG_FLAG_VM_API,   __VA_ARGS__)
#define DEBUG_VM_RUN(...)   PRINTF_DEBUG(DEBUG_FLAG_VM_RUN,   __VA_ARGS__)
// DEBUG facilities - end

// Libs
#include <utarray.h>
#include <uthash.h>

// Project
#include "ast.h"
#include "parser.h"
#include "obj.h"
#include "vm.h"
#include "compile.h"
#include "decompile.h"

#endif
