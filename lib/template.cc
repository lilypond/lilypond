/*
  template.cc -- instantiate Pointer_list<Source_file*>

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "config.hh"
#include "source-file.hh"
#include "plist.tcc"
#include "pcursor.tcc"
#include "cursor.tcc"
#include "list.tcc"

#if defined NEED_EXPLICIT_INSTANTIATION || __CYGWIN__
// huh?
//LIST_INSTANTIATE (void *);
template class List<void*>;
template class Cursor<void*>;
#endif

POINTERLIST_INSTANTIATE (Source_file);

