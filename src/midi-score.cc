//
// midi-score.cc -- implement Midi_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "moment.hh"
#include "duration.hh"
#include "sourcefile.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors
#include "midi-event.hh"
#include "lily-stream.hh"
#include "track-column.hh"
#include "midi-track.hh"
#include "midi-score.hh"

Midi_score::Midi_score( int format_i, int tracks_i, int tempo_i )
{
	format_i_ = format_i;
	tracks_i_ = tracks_i;
	tempo_i_ = tempo_i;
}

Midi_score::~Midi_score()
{
}

void
Midi_score::add_track( Midi_track* midi_track_p )
{
	midi_track_p_list_.bottom().add( midi_track_p );
}

int
Midi_score::output_mudela( String filename_str )
{
	mtor << "Lily output to " << filename_str << " ..." << endl;

	Lily_stream lily_stream( filename_str );
	for ( PCursor<Midi_track*> midi_track_l_pcur( midi_track_p_list_.top() ); midi_track_l_pcur.ok(); midi_track_l_pcur++ ) {
		midi_track_l_pcur->output_mudela( lily_stream );
		lily_stream << "\n";
	}

	lily_stream << "score {\n";

	for ( PCursor<Midi_track*> midi_track_l_pcur( midi_track_p_list_.top() ); midi_track_l_pcur.ok(); midi_track_l_pcur++ ) {
		lily_stream << "\tstaff { melodic music { ";
		lily_stream << midi_track_l_pcur->name_str();
		lily_stream << " } }\n";
	}

	lily_stream << "\tcommands { meter { 4*4 } }\n";
	lily_stream << "\tmidi { tempo 4:60 }\n";

	lily_stream << "}\n";

	return 0;
}

