/*
  template.cc -- instantiate Pointer_list<Source_file*>

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "config.hh"
#include "source-file.hh"
#include "plist.tcc"
#include "pcursor.tcc"
#include "cursor.tcc"
#include "list.tcc"

#ifdef NEED_EXPLICIT_INSTANTIATION
LIST_INSTANTIATE (void *);
#endif

POINTERLIST_INSTANTIATE (Source_file);

