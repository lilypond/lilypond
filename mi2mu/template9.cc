//
// template.cc -- implementemplate
// ugh: must have unique name for Cygnus' gcc:
// liblily.a(template.o): In function `GLOBAL_$I$template.cc':
// template.cc:28: multiple definition of `global constructors keyed to template.cc'
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "list.hh"
#include "list.tcc"
#include "cursor.tcc"

class istream;
class ostream;

#include "mudela-item.hh"
#include "mudela-column.hh"
#include "mudela-staff.hh"
#include "mudela-voice.hh"
#include "mudela-staff.hh"
#include "mudela-score.hh"
#include "pcursor.hh"
#include "plist.hh"
#include "pcursor.tcc"
#include "plist.tcc"

POINTERLIST_INSTANTIATE(Mudela_item);
POINTERLIST_INSTANTIATE(Mudela_staff);
POINTERLIST_INSTANTIATE(Mudela_voice);
POINTERLIST_INSTANTIATE(Mudela_column);
POINTERLIST_INSTANTIATE(Mudela_score);

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

