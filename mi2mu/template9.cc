//
// template.cc -- implementemplate
// ugh: must have unique name for Cygnus' gcc:
// liblily.a(template.o): In function `GLOBAL_$I$template.cc':
// template.cc:28: multiple definition of `global constructors keyed to template.cc'
//
// copyright 1997 Jan Nieuwenhuizen <janneke@gnu.org>

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
