//
// template.cc -- implementemplate
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "list.hh"
#include "list.tcc"
#include "cursor.tcc"

L_instantiate(void *);

class istream;
class ostream;

#include "mi2mu.hh"
#include "plist.hh"
#include "plist.tcc"

#ifdef MEVENT_LIST
IPL_instantiate(Midi_event);
#endif

IPL_instantiate(Midi_track);

#ifdef MVOICE_LIST
PL_instantiate(Midi_voice);
#endif

IPL_instantiate(Midi_voice);

#ifdef TCOL_LIST
IPL_instantiate(Track_column);
#endif

