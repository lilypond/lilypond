/*
  stringutil.cc -- generate non-inline members.

  This should be in a separate file, because one can include the .icc
  only once.

  source file of the LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl> */

#if !STD_STRING


#ifdef STRING_DEBUG
#define memmove mymemmove
#endif

#ifndef STRING_UTILS_INLINED

#ifdef INLINE
#undef INLINE
#endif

#define INLINE

#include <algorithm>

#include "string-data.hh"
#include "string-handle.hh"

#include "std-string.hh"

#include "string-data.icc"
#include "string-handle.icc"
#include "string.icc"

#ifdef STRING_DEBUG
#include <sys/types.h>
#include <memory>
using namespace std;

void *
mymemmove (void *dest, void const *src, size_t n)
{
  return memcpy (dest, src, n);
}
#endif

#endif /* STRING_UTILS_INLINED */

#endif /* !STD_STRING */
