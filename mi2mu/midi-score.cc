//
// midi-score.cc -- implement Midi_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

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
	tor( NORMAL_ver ) << "Lily output to " << filename_str << " ..." << endl;
	
	// ugh, ugly midi type 1 fix
	if ( ( midi_track_p_list_.size() == 1 ) && !midi_track_p_list_.top()->number_i_ )
		midi_track_p_list_.top()->number_i_ = 1;

	int track_i = 0;
	Lily_stream lily_stream( filename_str );
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ ) {
		tor( NORMAL_ver ) << "track " << track_i++ << ": " << flush;
		i->output_mudela( lily_stream );
		lily_stream << "\n";
		tor( NORMAL_ver ) << endl;
	}

	lily_stream << "\\score{\n";
	lily_stream << " < \\multi 3;\n";
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ ) {
		if ( ( midi_track_p_list_.size() != 1 ) 
			&& ( i == midi_track_p_list_.top() ) )
			continue;
		lily_stream << "\\melodic{ ";
		lily_stream << "\\$" << i->id_str();
		lily_stream << " }\n";
	}
	lily_stream << ">\n";

	lily_stream << "\\paper{";
	lily_stream << "unitspace = 20.0\\mm;";
	lily_stream << "}\n";

	lily_stream << "\\midi{";
		// not use silly 0 track
		midi_track_p_list_.bottom()->midi_tempo_p_->output_mudela( lily_stream, true );
	lily_stream << "}\n";

	lily_stream << "}\n";

	return 0;
}

void
Midi_score::process()
{
	int track_i = 0;
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ )  {
		tor( NORMAL_ver ) << "track " << track_i++ << ": " << flush;
		i->process();
		tor( NORMAL_ver ) << endl;
	}
}

