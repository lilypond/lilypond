/*
  stringutil.cc -- generate non-inline members. 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifdef STRING_DEBUG 
void* mymemmove( void* dest, void* src, size_t n )
{
	return memmove( dest, src, n ); // wohltempererit: 69006
}
#define memmove mymemmove
#endif

#ifdef STRING_UTILS_INLINED
#undef STRING_UTILS_INLINED
#endif

#ifdef INLINE
#undef INLINE
#endif

#define INLINE

#include "stringhandle.hh"
#include "stringdata.hh"
#include "stringdata.inl"
#include "stringhandle.inl"
