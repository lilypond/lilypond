//
// midi-template.cc -- implementemplate
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "list.tcc"
#include "cursor.tcc"

L_instantiate(void *);

class istream;
class ostream;
#include "proto.hh"
#include "plist.tcc"
#include "pcursor.tcc"
#include "string.hh"
#include "source-file.hh"

#include "moment.hh"
#include "duration.hh"
#include "midi-event.hh"
#include "lily-stream.hh"
#include "track-column.hh"
#include "midi-track.hh"

IPL_instantiate(Midi_event);
IPL_instantiate(Midi_track);
IPL_instantiate(Source_file);
IPL_instantiate(Track_column);
