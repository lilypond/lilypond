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

//IPL_instantiate(Midi_event);
IPL_instantiate(Midi_track);
// PL_instantiate(Midi_voice);
// IPL_instantiate(Midi_voice);
//IPL_instantiate(Track_column);

