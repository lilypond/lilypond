/*
  duration-convert.cc -- implement 

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include <assert.h>
#include "duration-convert.hh"
#include "debug.hh"

// statics Duration_convert
bool const Duration_convert::midi_as_plet_b_s = true;
bool Duration_convert::no_quantify_b_s = false;
bool Duration_convert::no_double_dots_b_s = false;
bool Duration_convert::no_triplets_b_s = false;
int Duration_convert::no_smaller_than_i_s = 0;
	
String 
Duration_convert::dur2_str( Duration dur )
{
	if ( dur.ticks_i_ )
		return String( "["  ) + String( dur.ticks_i_ ) + "]";

	String str( dur.type_i_ );
	str += String( '.', dur.dots_i_ );
	if ( dur.plet_b())
		str += String( "*" ) + String( dur.plet_.iso_i_ )
			+ String( "/" ) + String( dur.plet_.type_i_ );
	return str;
}

#if 0
int
Duration_convert::dur2_i( Duration dur, int division_1_i )
{
	return dur2_mom( dur ) * Moment( division_1_i );
}
#endif

int
Duration_convert::dur2ticks_i( Duration dur )
{
	if ( dur.ticks_i_ )
		return dur.ticks_i_;
	return dur2_mom( dur ) * Moment( Duration::division_1_i_s );
}

Moment
Duration_convert::dur2_mom( Duration dur )
{
	if ( dur.ticks_i_ )
		return Moment( dur.ticks_i_, Duration::division_1_i_s );	

	// or simply assert?
	if ( !dur.type_i_ )
		return Moment( 0 );

	Moment mom = Moment( 1 , dur.type_i_ );

	Moment delta = mom;
	while ( dur.dots_i_-- ) {
		delta /= 2.0;
		mom += delta;
	}

	return mom * plet_factor_mom( dur );    
}

#if 0
Moment
Duration_convert::i2_mom( int time_i, int division_1_i )
{
	if ( !time_i )
		return Moment( 0 );

	if ( division_1_i > 0 )
		return Moment( time_i, division_1_i );
	else 
		return Moment( -division_1_i, time_i );
}
#endif

Duration
Duration_convert::mom2_dur( Moment mom )
{
	/* this is cute, 
	   but filling an array using Duration_iterator
	   might speed things up, a little
	   */
  	Duration_iterator iter_dur;
  	assert( iter_dur );
  	while ( iter_dur ) {
		Duration dur = iter_dur++;
		if ( mom == dur2_mom( dur ) )
			return dur;
	}
	if ( midi_as_plet_b_s ) {
		Moment mom_4 = mom / Moment( 4 );
		long num = mom_4.numerator().as_long();
		long den = mom_4.denominator().as_long();
		Duration dur( 4, 0 );
		dur.set_plet( num, den );
		return dur;
	}
	assert( 0 );
	// no can do
	Duration dur( 0 );
	return dur;
}

Duration
Duration_convert::mom2standardised_dur( Moment mom )
{
	/* this is cute, 
	   but filling an array using Duration_iterator
	   might speed things up, a little
	   */
	Duration_iterator iter_dur;
	assert( iter_dur );
	while ( iter_dur ) {
  		Duration lower_dur = iter_dur++;
  		Duration upper_dur( 0 );
  		if ( iter_dur )
  			upper_dur = iter_dur();
  		Moment lower_mom = dur2_mom( lower_dur );
  		Moment upper_mom = dur2_mom( upper_dur );
		if ( mom < lower_mom )
			return lower_dur;
  		if ( mom == lower_mom )
  			return lower_dur;
	}
	return iter_dur();
}


Moment
Duration_convert::plet_factor_mom( Duration dur )
{
	return dur.plet_.mom();
}

Real
Duration_convert::sync_f( Duration dur, Moment mom )
{
	return mom / dur2_mom( dur );
}

Duration
Duration_convert::ticks2_dur( int ticks_i )
{
	/* this is cute, 
	   but filling an array using Duration_iterator
	   might speed things up, a little
	   */
	// should use mom2_dur
	Moment mom( ticks_i, Duration::division_1_i_s );
	Duration_iterator iter_dur;
	assert( iter_dur );
	while ( iter_dur ) {
		Duration dur = iter_dur++;
		if ( mom == dur2_mom( dur ) )
			return dur;
	}
	if ( midi_as_plet_b_s ) {
		Duration dur( 4, 0 );
		dur.set_plet( ticks_i, Duration::division_1_i_s / 4 ); 
		return dur;
	}
	Duration dur( 0 );
	dur.set_ticks( ticks_i );
	return dur;
}

Duration
Duration_convert::ticks2standardised_dur( int ticks_i )
{
	/* this is cute, 
	   but filling an array using Duration_iterator
	   might speed things up, a little
	   */
	// should use mom2standardised_dur
	Moment mom( ticks_i, Duration::division_1_i_s );
	Duration_iterator iter_dur;
	assert( iter_dur );
	while ( iter_dur ) {
		Duration lower_dur = iter_dur++;
//		Duration upper_dur( 0 );
		Duration upper_dur( 1, 1 );
		if ( iter_dur )
			upper_dur = iter_dur();
		Moment lower_mom = dur2_mom( lower_dur );
		Moment upper_mom = dur2_mom( upper_dur );
		if ( mom < lower_mom )
			return lower_dur;
		if ( mom == lower_mom )
			return lower_dur;
		if ( mom == upper_mom ) // don-t miss last (sic)
			return upper_dur;
		if ( ( mom >= lower_mom ) && ( mom <= upper_mom ) ) {
			warning( String( "duration not exact: " ) + String( (Real)mom ), 0 );
			if ( abs( mom - lower_mom ) < abs( mom - upper_mom ) )
				return lower_dur;
			else
				return upper_dur;
		}
		lower_dur = upper_dur;
	}
	return iter_dur();
}

Duration_iterator::Duration_iterator()
{
	cursor_dur_.type_i_ = 128;
	if ( Duration_convert::no_smaller_than_i_s )
		cursor_dur_.type_i_ = Duration_convert::no_smaller_than_i_s;
//	cursor_dur_.set_plet( 1, 1 );
}

Duration 
Duration_iterator::operator ++(int)
{
	return forward_dur();
}

Duration
Duration_iterator::operator ()()
{
	return dur();
}

Duration_iterator::operator bool()
{
	return ok();
}

Duration
Duration_iterator::dur()
{
	return cursor_dur_;
}

Duration
Duration_iterator::forward_dur()
{
	/* should do smart table? guessing: 
		duration wholes
		16 	0.0625
		32.. 	0.0703
		8:2/3	0.0833
		16.	0.0938
		8	0.1250
		16..	0.1406
		4:2/3	0.1667
		8.	0.1875
		
	*/
	assert( ok() );

	Duration dur = cursor_dur_;

	if ( !cursor_dur_.dots_i_ && !cursor_dur_.plet_b() ) {
		cursor_dur_.type_i_ *= 2;
		cursor_dur_.dots_i_ = 2;
	}
	else if ( cursor_dur_.dots_i_ == 2 ) {
		assert( !cursor_dur_.plet_b() );
		cursor_dur_.dots_i_ = 0;
		cursor_dur_.type_i_ /= 4;
		cursor_dur_.set_plet( 2, 3 );
	}
	else if ( cursor_dur_.plet_b() 
		&& ( cursor_dur_.plet_.iso_i_ == 2 )
		&& ( cursor_dur_.plet_.type_i_ == 3 ) ) {
		assert( !cursor_dur_.dots_i_ );
		cursor_dur_.set_plet( 1, 1 );
		cursor_dur_.type_i_ *= 2;
		cursor_dur_.dots_i_ = 1;
	}
	else if ( cursor_dur_.dots_i_ == 1 ) {
		assert( !cursor_dur_.plet_b() );
		cursor_dur_.dots_i_ = 0;
		cursor_dur_.type_i_ /= 2;
	}
		
	if ( Duration_convert::no_triplets_b_s
	     && cursor_dur_.plet_b() && ok() )
	    forward_dur();
	if ( Duration_convert::no_double_dots_b_s 
	     && ( cursor_dur_.dots_i_ == 2 ) && ok() )
		forward_dur();
	if ( Duration_convert::no_smaller_than_i_s
	     && ( cursor_dur_.type_i_ > Duration_convert::no_smaller_than_i_s ) && ok() )
		forward_dur();
	if ( Duration_convert::no_smaller_than_i_s
	     && cursor_dur_.dots_i_
	     && ( cursor_dur_.type_i_ >= Duration_convert::no_smaller_than_i_s )
	     && ok() )
		forward_dur();
	if ( Duration_convert::no_smaller_than_i_s
	     && ( cursor_dur_.dots_i_ == 2 )
	     && ( cursor_dur_.type_i_ >= Duration_convert::no_smaller_than_i_s / 2 )
	     && ok() )
		forward_dur();

	return dur;
}

bool
Duration_iterator::ok()
{
	return ( cursor_dur_.type_i_ 
		&& !( ( cursor_dur_.type_i_ == 1 ) && ( cursor_dur_.dots_i_ > 2 ) ) );
}
