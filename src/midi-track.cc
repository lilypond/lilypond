//
// midi-track.cc -- implement Midi_track
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "source-file.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors

#include "moment.hh"
#include "duration.hh"
#include "midi-event.hh"
#include "lily-stream.hh"
#include "track-column.hh"
#include "midi-track.hh"

Midi_track::Midi_track( int track_i )
{
	name_str_ = String( "track" ) + String( track_i );
	tcol_p_list_.bottom().add( new Track_column( Moment( 0 ) ) );
}

Midi_track::~Midi_track()
{
}

void
Midi_track::add_event( Moment mom, Midi_event* midi_event_p )
{
	if ( ! midi_event_p )
		return;
	tcol_l( mom - midi_event_p->mom() )->add_event( midi_event_p );
}

// too much red tape ?
String
Midi_track::name_str()
{
	return name_str_;
}

void
Midi_track::output_mudela( Lily_stream& lily_stream_r )
{
	lily_stream_r << name_str_ << " = music { $\n";
	lily_stream_r << "\t";
	int column_i = 8;

	for ( PCursor<Track_column*> tcol_l_pcur( tcol_p_list_.top() ); tcol_l_pcur.ok(); tcol_l_pcur++ ) {
		if ( tcol_l_pcur->midi_event_p_list_.size() > 1 )
			warning( "oeps, chord: can-t do that yet", 0 );
		if ( !tcol_l_pcur->midi_event_p_list_.size() )
			continue;
		lily_stream_r << **tcol_l_pcur->midi_event_p_list_.top();
		column_i += tcol_l_pcur->midi_event_p_list_.top()->mudela_str().length_i();
		if ( column_i > 40 ) {
			lily_stream_r << "\n\t";
			column_i = 8;
		}
	}
	lily_stream_r << "\n$} % " << name_str_ << "\n";
}

Track_column*
Midi_track::tcol_l( Moment mom )
{
	for ( PCursor<Track_column*> tcol_l_pcur( tcol_p_list_.top() ); tcol_l_pcur.ok(); tcol_l_pcur++ ) {
    		if ( tcol_l_pcur->mom() == mom )
			return *tcol_l_pcur;
		if ( tcol_l_pcur->mom() > mom ) {
			Track_column* tcol_p = new Track_column( mom );
			tcol_l_pcur.insert( tcol_p );
			return tcol_p;
		}
	}

	Track_column* tcol_p = new Track_column( mom );
	tcol_p_list_.bottom().add( tcol_p );
	return tcol_p;
}

