//
// midi-score.cc -- implement Midi_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "m2m.hh"

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
		lily_stream.newline();
	}

	lily_stream << "score {";
	lily_stream.newline();

	for ( PCursor<Midi_track*> midi_track_l_pcur( midi_track_p_list_.top() ); midi_track_l_pcur.ok(); midi_track_l_pcur++ ) {
		lily_stream << "\tstaff { melodic music { ";
		lily_stream << midi_track_l_pcur->name_str();
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
	for ( PCursor<Midi_track*> i( midi_track_p_list_.top() ); i.ok(); i++ ) 
		i->process();
}

