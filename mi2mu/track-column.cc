//
// track-column.cc -- implement Track_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

Track_column::Track_column( Moment mom )
{
	mom_ = mom;
}

void 
Track_column::add_event( Midi_event* midi_event_p )
{
#ifdef MEVENT_LIST
	midi_event_p_list_.bottom().add( midi_event_p );
#else
	midi_event_p_array_.push( midi_event_p );
#endif
}

Moment
Track_column::mom()
{
	return mom_;
}
