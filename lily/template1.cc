/*
  template1.cc -- instantiate some List classes

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "idealspacing.hh"
#include "plist.hh"
#include "p-col.hh"
#include "item.hh"
#include "musical-request.hh"
#include "spanner.hh"
#include "cursor.tcc"
#include "pcursor.tcc"
#include "plist.tcc"


#define IPLC_INSTANTIATE(a) POINTERLIST_INSTANTIATE(a)

IPLC_INSTANTIATE(Score_elem);
IPLC_INSTANTIATE(Spanner);
IPLC_INSTANTIATE(Idealspacing);

/* these are required at least on IRIX 5 and SunOS 4 */

#if 0
/* 
   what gcc version?
   they're in lib/template.cc too, and break linux/gcc-2.7.2 
   are you sure they must be here too?

   for now, i'll just change the switch in lib/template.cc
   jcn
 */

#if defined (__linux__) && && __GNUC_MINOR__ < 8

#include "list.tcc"

// template class List<void *>;
// template class Cursor<void *>;

// this should work too, and
LIST_INSTANTIATE (void *);

#endif

#endif
