//
// template.cc -- implementemplate
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
#include "plist.hh"
#include "plist.tcc"

template IPL_INSTANTIATE(Mudela_item);
template IPL_INSTANTIATE(Mudela_staff);
template PL_INSTANTIATE(Mudela_voice);
template IPL_INSTANTIATE(Mudela_voice);
template IPL_INSTANTIATE(Mudela_column);

