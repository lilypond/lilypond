//
// midi-event.cc -- implement Midi_event
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <assert.h>
#include "proto.hh"

#include "plist.hh"      // all for midi-main.hh
#include "string.hh"
#include "source.hh"
#include "sourcefile.hh"
#include "midi-main.hh"  // *tors

#include "moment.hh"
#include "midi-event.hh"

Midi_event::Midi_event()
{
}

Midi_event::~Midi_event()
{
}

String
Midi_event::mudela_str()
{
	return mudela_str_;
}

Midi_key::Midi_key( int accidentals_i, int minor_i )
{
	accidentals_i_ = accidentals_i;
	minor_i_ = minor_i;
	if ( !minor_i_ )
		key_i_ = ( ( accidentals_i % 7 )[ "cgdaebf" ] - 'a' + 2 ) % 7;
	else
		key_i_ = ( ( -accidentals_i % 7 )[ "fbeadg" ] - 'a' + 2 ) % 7;
	mudela_str_ = String( "\\key\\" );
	if ( !minor_i_ ) 
		mudela_str_ += String( (char)( key_i_ - 2 + 'A'  ) );
	else
		mudela_str_ += String( (char)( key_i_ - 2 + 'a'  ) );
}

String
Midi_key::notename_str( int pitch_i )
{
	// this may seem very smart,
	// but it-s only an excuse not to read a notename table

	// major scale: do-do
	// minor scale: la-la ( = + 5 )
	static int notename_i_a[ 12 ] = { 0, 0, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6 };
	int notename_i = notename_i_a[ ( minor_i_ * 5 + pitch_i ) % 12 ];
	
	static int accidentals_i_a[ 12 ] = { 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0 };
	int accidental_i = accidentals_i_a[ minor_i_ * 5 + pitch_i % 12 ];
	if ( accidentals_i_ < 0 ) {
		accidental_i = - accidental_i;
		notename_i = ( notename_i + 1 ) % 7;
	}

	String notename_str = (char)( ( ( notename_i + key_i_ - 2 ) % 7 ) + 'a' );
	while ( accidental_i-- > 0 )
		notename_str += "is";
	accidental_i++;
	while ( accidental_i++ < 0 )
		if ( ( notename_str == "a" ) || ( notename_str == "e" ) )
			notename_str += "s";
		else
			notename_str += "es";
	accidental_i--;
	return notename_str;
}

Midi_key::~Midi_key()
{
}

Midi_note::Midi_note( Midi_key* midi_key_l, Midi_time* midi_time_l, int clocks_per_whole_i, int pitch_i, Real delta_time_f )
{
	mudela_str_ = midi_key_l->notename_str( pitch_i );
	mudela_str_ += midi_time_l->duration_str( clocks_per_whole_i, delta_time_f );
}

Midi_note::~Midi_note()
{
}

Midi_tempo::Midi_tempo( int useconds_per_4_i )
{
	useconds_per_4_i_ = useconds_per_4_i;
	seconds_per_1_f_ = (Real)useconds_per_4_i_ * 4 / 1e6;
	mudela_str_ = "\\Tempo: ";
	mudela_str_ += String( useconds_per_4_i_ );
	mudela_str_ += String( ": " ) 
		+ String( get_tempo_i( Moment( 1, 4 ) ) )
		+ String( " 4 per minute" );
}

Midi_tempo::~Midi_tempo()
{
}

int
Midi_tempo::get_tempo_i( Moment moment )
{
	return Moment( 60 ) / moment / Moment( seconds_per_1_f_ );
}

Midi_time::Midi_time( int num_i, int den_i, int clocks_i, int count_32_i )
{
//	we should warn, at least!
//	assert( count_32_i == 8 );
	if ( count_32_i != 8 )
		warning( String( "#32 in quarter: " ) + String( count_32_i ), 0 );
	num_i_ = num_i;
	den_i_ = 2 << den_i;
	// huh?, see midiitem.cc; reasonably for fugue1.midi
//	whole_clocks_i_ = clocks_i * 4 * 10; 
	whole_clocks_i_ = clocks_i * 4; 
//	whole_clocks_i_ = clocks_i; 
	mudela_str_ = "\\Time: ";
	mudela_str_ += String( num_i_ ) + "/" + String( den_i_ )
		+ ", " + String( whole_clocks_i_ )
		+ ": " + String( count_32_i );
}

Midi_time::~Midi_time()
{
}

String
Midi_time::duration_str( int clocks_per_whole_i, Real delta_time_f )
{
	dtor << "(" << delta_time_f;
	String str;

	Moment delta_time_moment = 1.0;
	if ( delta_time_f ) {
		// 288/96*.25 = .75
		// 96/96*.25 = .25
		// 24/96*.25 = 0.0625
//		delta_time_moment = Moment( delta_time_f ) / Moment( whole_clocks_i() );
		if ( clocks_per_whole_i > 0 )
			delta_time_moment = Moment( delta_time_f ) / Moment( clocks_per_whole_i );
		else 
			delta_time_moment = Moment( -clocks_per_whole_i ) / Moment( delta_time_f );
	}		

	dtor << ", " << (Real)delta_time_moment << "): ";

	static Real sync_f = 1;
	delta_time_moment = delta_time_moment / sync_f;

	double dummy_f;
	if ( ( sync_f == 1 ) 
		&& ( abs( modf( (Real)delta_time_moment / ( (Real)1 / 64 / 3 ), &dummy_f ) ) ) > 1e-6 ) {
		sync_f = (Real)delta_time_moment / 0.125;
		delta_time_moment = (Real)delta_time_moment / sync_f;
		mtor << "\nsyncing: " << sync_f << ", "
//			<< (Real)sync_f / usecond24th_per_clock_i << endl;
			<< (Real)whole_clocks_i() << endl;
	}

	static bool must_resync_bo = false;
	if ( ( (Real)delta_time_moment / 0.125 > 16 )
		|| ( (Real)delta_time_moment / 0.125 < ( (Real)1 / 16 ) ) ) 
		must_resync_bo = true;
	
	if ( must_resync_bo ) {
		must_resync_bo = false;
		delta_time_moment = delta_time_moment * sync_f;
		sync_f = (Real)delta_time_moment / 0.125;
		delta_time_moment = (Real)delta_time_moment / sync_f;
		mtor << "\nresyncing: " << sync_f << ", "
//			<< (Real)sync_f / usecond24th_per_clock_i << endl;
			<< (Real)whole_clocks_i() << endl;
	}

	if ( !delta_time_moment ) 
		return "0";

	// output of modf: double 
	double duration_f = 0;
	Real rest_f = modf( ( Moment( 1 ) / delta_time_moment ), &duration_f );
	int dots_i = 0;
	int plet_type_i = 0;
	if ( rest_f ) {
		// output of modf: double 
		double dots_f = 0;
		Real dots_rest_f = modf( rest_f * 3, &dots_f );
		if ( !dots_rest_f )
			dots_i = (int)rint( dots_f );
		else if ( abs( dots_rest_f - rint( dots_rest_f ) ) < 1e-8 )
			dots_i = (int)rint( dots_rest_f );
		// try 3plet
		else if ( !modf( (Real)3 * delta_time_moment / 2, &duration_f ) )
			plet_type_i = 3;
		// try 6plet
		else if ( !modf( (Real)6 * delta_time_moment / 2, &duration_f ) )
			plet_type_i = 6;
		else {
//			str += String( "\n*" ) + String( (int)( 1 / rest_f ) );
			str += String( "\n*" ) + String( rest_f );
			must_resync_bo = true;
		}
	}

	if ( dots_i ) {
		for ( int i = dots_i; i ; i-- )
			delta_time_moment *= (Real)2 / 3;
		modf( ( Moment( 1 ) / delta_time_moment ), &duration_f );
	}

	str += String( (int)( duration_f ) );
	if ( dots_i )
		str += String( '.', dots_i );
	if ( plet_type_i )
		str += String( "/" ) + String( plet_type_i );

	return str;
}

int
Midi_time::whole_clocks_i()
{
	return whole_clocks_i_;
}

