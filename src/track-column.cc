//
// track-column.cc -- implement Track_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "moment.hh"
#include "duration.hh"
#include "midi-event.hh"
#include "lily-stream.hh"
#include "track-column.hh"
#include "midi-track.hh"

Track_column::Track_column( Moment mom )
{
	mom_ = mom;
}

Track_column::~Track_column()
{
}

void 
Track_column::add_event( Midi_event* midi_event_p )
{
	midi_event_p_list_.bottom().add( midi_event_p );
}

Moment
Track_column::mom()
{
	return mom_;
}
