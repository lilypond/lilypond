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
	mtor << "Lily output to " << filename_str << " ..." << endl;

	int track_i = 0;
	Lily_stream lily_stream( filename_str );
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ ) {
		mtor << "track " << track_i++ << ": " << flush;
		i->output_mudela( lily_stream );
		lily_stream.newline();
		mtor << endl;
	}

	lily_stream << "score {";
	lily_stream.newline();

	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ ) {
		lily_stream << "\tstaff { melodic music { ";
		lily_stream << i->name_str();
		lily_stream << " } }";
		lily_stream.newline();
	}

	lily_stream.indent();
		lily_stream << "commands {";
		lily_stream.indent();
			midi_parser_l_g->midi_time_p_->output_mudela( lily_stream, true );
			lily_stream.tnedni();
		lily_stream << "}";
		lily_stream.newline();
		lily_stream << "midi {";
			lily_stream.indent();
			midi_parser_l_g->midi_tempo_p_->output_mudela( lily_stream, true );
			lily_stream.tnedni();
		lily_stream << "}";
		lily_stream.tnedni();

	lily_stream << "}";
	lily_stream.newline();

	return 0;
}

void
Midi_score::process()
{
	int track_i = 0;
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ )  {
		mtor << "track " << track_i++ << ": " << flush;
		i->process();
		mtor << endl;
	}
}

