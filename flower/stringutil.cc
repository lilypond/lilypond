/*
  stringutil.cc -- generate non-inline members. 

  This should be in a separate file, because one can include the .icc
  only once.

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl> */

#ifdef STRING_DEBUG 
#define memmove mymemmove
#endif

#ifdef STRING_UTILS_INLINED
#undef STRING_UTILS_INLINED
#endif

#ifdef INLINE
#undef INLINE
#endif

#define INLINE

#include "string-handle.hh"
#include "string-data.hh"
#include "string.hh"
#include "string-data.icc"
#include "string-handle.icc"
#include "string.icc"

#ifdef STRING_DEBUG
#include <sys/types.h>
#include <memory.h>
void* 
mymemmove (void* dest, void const* src, size_t n)
{
	return memcpy (dest, src, n); 
}
#endif
