/*
  template.cc -- instantiate Pointer_list<Source_file*>

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "source-file.hh"
#include "plist.tcc"
#include "pcursor.tcc"
#include "cursor.tcc"
#include "list.tcc"

#if !defined(__CYGWIN32__) && __GNUC_MINOR__ < 8
LIST_INSTANTIATE (void *);
#endif

POINTERLIST_INSTANTIATE (Source_file);
