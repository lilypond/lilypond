//
// my-midi-parser.cc -- implement My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"

#include "plist.hh"        // ugh
#include "string.hh"
#include "sourcefile.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors

#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "midi-event.hh"
#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"

void
yyerror(char const* sz_l )
{
	midi_parser_l_g->error( sz_l );
}


My_midi_parser* midi_parser_l_g;

My_midi_parser::My_midi_parser( String filename_str )
{
	midi_lexer_p_ = new My_midi_lexer( filename_str );
	midi_parser_l_g = this;

	midi_key_p_ = new Midi_key( 0, 0 );
//	midi_tempo_p_ = new Midi_tempo( 384 ); // wiellie dunno!
	// 07A120 == 500000
	midi_tempo_p_ = new Midi_tempo( 0x07a120 ); // wiellie dunno!
	midi_time_p_ = new Midi_time( 4, 4, 0x24, 8 );

	defined_ch_c_l_ = 0;
	fatal_error_i_ = 0;
	now_f_ = 0;
	step_f_ = 0;

	for ( int i = 0; i < CHANNELS_i; i++ )
		for ( int j = 0; j < PITCHES_i; j++ )
			running_f_f_a_[ i ][ j ] = 0;
}

My_midi_parser::~My_midi_parser()
{
	delete midi_lexer_p_;
	midi_parser_l_g = 0;
	delete midi_key_p_;
	delete midi_tempo_p_;
	delete midi_time_p_;
}

void
My_midi_parser::add_score( Midi_score* midi_score_p )
{
	midi_score_p_array_.push( midi_score_p );
	cout << endl;
}

void
My_midi_parser::error( char const* sz_l )
{
	midi_lexer_l_g->error( sz_l );

	if ( fatal_error_i_ )
		exit( fatal_error_i_ );
}

void
My_midi_parser::forward( Real f )
{
	// ugh
	if ( f )
		step_f_ = f;
	now_f_ += step_f_;
}

void
My_midi_parser::note_begin( int channel_i, int pitch_i, int dyn_i )
{
	// one pitch a channel at time!
	//  heu, what if start at t = 0?
//	assert( !running_f_f_a_[ channel_i ][ pitch_i ] );
	running_f_f_a_[ channel_i ][ pitch_i ] = now_f_;
}

Midi_event*
My_midi_parser::note_end_midi_event_p( int channel_i, int pitch_i, int dyn_i )
{
	Real start_f = running_f_f_a_[ channel_i ] [ pitch_i ];
	// did we start? -> heu, don-t know: what if start at t = 0?
//	assert( start_f ); 
	return new Midi_note( midi_key_p_, midi_time_p_, clocks_per_whole_i_, pitch_i, now_f_ - start_f );
}

int
My_midi_parser::output( String filename_str )
{
	return 0;
}

int
My_midi_parser::parse()
{
	return ::yyparse();
}

void
My_midi_parser::set_division( int clocks_per_4_i )
{
	clocks_per_whole_i_ = clocks_per_4_i * 4;
	if ( clocks_per_4_i < 0 )
		warning( "seconds iso metrical time" , 0 );
}

void
My_midi_parser::set_key( int accidentals_i, int minor_i )
{
	delete midi_key_p_;
	midi_key_p_ = new Midi_key( accidentals_i, minor_i );
}

void
My_midi_parser::set_tempo( int useconds_i )
{
	delete midi_tempo_p_;
	midi_tempo_p_ = new Midi_tempo( useconds_i );
}

void
My_midi_parser::set_time( int num_i, int den_i, int clocks_i, int count_32_i )
{
	delete midi_time_p_;
	midi_time_p_ = new Midi_time( num_i, den_i, clocks_i, count_32_i );
}

