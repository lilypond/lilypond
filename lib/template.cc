/*
  template.cc -- instantiate Pointer_list<Source_file*>

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "source-file.hh"
#include "plist.tcc"
#include "pcursor.tcc"
#include "cursor.tcc"
#include "list.tcc"

#if 0
LIST_INSTANTIATE (void *);

#else

    static void force_list_members ()
    {
    List<void*> bla;
    bla.top().add ((void*)0);
    }

#endif

POINTERLIST_INSTANTIATE (Source_file);
