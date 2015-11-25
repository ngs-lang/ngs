#include "ngs.h"

#ifdef NGS_DEBUG_FLAGS
#define NGS_DEBUG_FLAGS
#define DO_NGS_DEBUG
uint32_t debug_flags = NGS_DEBUG_FLAGS;
#else
uint32_t debug_flags = 0x0000;
#endif
