/*
  stringutil.cc -- generate non-inline members. 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


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
