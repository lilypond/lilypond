//
// my-midi-parser.cc -- implement My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"

#include "plist.hh"        // ugh
#include "string.hh"
#include "source-file.hh"
#include "source.hh"
#include "midi-main.hh"    // *tors

#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "duration.hh"
#include "midi-event.hh"
#include "lily-stream.hh"
#include "track-column.hh"
#include "midi-track.hh"
#include "midi-score.hh"
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
	defined_ch_c_l_ = 0;
	fatal_error_i_ = 0;
	midi_key_p_ = 0;
	midi_score_p_ = 0;
	midi_tempo_p_ = 0;
	midi_time_p_ = 0;
	reset();
}

void
My_midi_parser::reset()
{
	delete midi_key_p_;
	midi_key_p_ = new Midi_key( 0, 0 );
	// useconds per 4: 250000 === 60 4 per minute
	delete midi_tempo_p_;
	midi_tempo_p_ = new Midi_tempo( 250000 );
	delete midi_time_p_;
	midi_time_p_ = new Midi_time( 4, 4, 384, 8 );

	now_i64_ = 0;

	for ( int i = 0; i < CHANNELS_i; i++ )
		for ( int j = 0; j < PITCHES_i; j++ )
//			running_i64_i64_a_[ i ][ j ] = -1;
			running_i64_i64_a_[ i ][ j ] = 0;
}

My_midi_parser::~My_midi_parser()
{
	delete midi_lexer_p_;
	midi_parser_l_g = 0;
	delete midi_key_p_;
	delete midi_tempo_p_;
	delete midi_time_p_;
	delete midi_score_p_;
}

void
My_midi_parser::add_score( Midi_score* midi_score_p )
{
	assert( !midi_score_p_ );
	midi_score_p_ = midi_score_p;
}

void
My_midi_parser::error( char const* sz_l )
{
	midi_lexer_l_g->error( sz_l );

	if ( fatal_error_i_ )
		exit( fatal_error_i_ );
}

void
My_midi_parser::forward( int i )
{
	now_i64_ += i;
}

Moment
My_midi_parser::mom()
{
	return Duration_convert::i2_mom( now_i64_, division_1_i_ );
}

void
My_midi_parser::note_begin( int channel_i, int pitch_i, int dyn_i )
{
	// one pitch a channel at time!
	// heu, what about { < c2 >  < c4 d4 > }
//	assert( running_i64_i64_a_[ channel_i ][ pitch_i ]  == -1 );
	running_i64_i64_a_[ channel_i ][ pitch_i ] = now_i64_;
}

Midi_event*
My_midi_parser::note_end_midi_event_p( int channel_i, int pitch_i, int dyn_i )
{
	Int64 start_i64 = running_i64_i64_a_[ channel_i ][ pitch_i ];
//	running_i64_i64_a_[ channel_i ][ pitch_i ] = -1;
	// did we start?
//	assert( start_i64 != -1 ); 
	return new Midi_note( midi_key_p_, midi_time_p_, division_1_i_, pitch_i, now_i64_ - start_i64 );
}

int
My_midi_parser::output_mudela( String filename_str )
{
	assert( midi_score_p_ );
	return midi_score_p_->output_mudela( filename_str );
}

int
My_midi_parser::parse()
{
	return ::yyparse();
}

void
My_midi_parser::set_division_4( int division_4_i )
{
	division_1_i_ = division_4_i * 4;
	if ( division_4_i < 0 )
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

