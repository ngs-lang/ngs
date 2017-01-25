#ifndef DEBUG_H
#define DEBUG_H

#ifdef NGS_DEBUG_FLAGS

#define DO_NGS_DEBUG
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

#else

#define PRINTF_DEBUG(...)
#define IF_DEBUG(...)
#define DEBUG_PARSER(...)
#define DEBUG_COMPILER(...)
#define DEBUG_VM_API(...)
#define DEBUG_VM_RUN(...)

#endif


#endif
