/*
	  duration-convert.hh -- declare 

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DURATION_CONVERT_HH
#define DURATION_CONVERT_HH
#include "duration.hh"
#include "string.hh"
//#include "array.hh"
#include "array.hh"

/**
	Duration_convert handles all conversions to -n fro Duration (dur).
	That is including (integer + division) representation for MIDI,
	and conversion from unexact time representation (best guess :-).

	A Moment (mom) is a Rational that holds the time fraction 
	compared to a whole note (before also called wholes).

	SUGGESTION: currently a moment in time is called moment too;
	let-s typedef Rational When too, so that we get 
	When Staff_column::when (), Moment Voice_element::mom ().

	[todo]
	move all statics to real members, instantiate Duration_convert
	object (s).
*/
struct Duration_convert {
	
  /* Urgh. statics.
   */
  static bool const midi_as_plet_b_s;
  static bool no_quantify_b_s;
  static bool no_double_dots_b_s;
  static bool no_triplets_b_s;
  static int no_smaller_than_i_s;
  static Array<Duration> dur_array_s;

  /// Return number of ticks in (ticks, division_1) representation
  static int dur2ticks_i (Duration dur );
	
  /// Return the type_i representation of note length i
  static int i2_type (int i);

  /// Return the note length corresponding to the type_i representation
  /// Return 0 if longer than whole note.
  static int type2_i (int type);

  /// Return Moment representation (fraction of whole note).
  static Moment dur2_mom (Duration dur );

  /// Return Mudela string representation.
  static String dur2_str (Duration dur );

  /// Return duration from Moment (fraction of whole) representation.
  static Duration mom2_dur (Moment mom );

  /// Return standardised duration, best guess if not exact.
  static Duration mom2standardised_dur (Moment mom );
  
  /// Return plet factor (not a Moment: should use Rational?).
  static Moment plet_factor_mom (Duration dur );

  static void set_array ();

  /** Return synchronisation factor for mom, so that
      mom2_dur (mom / sync_f ) will return the duration dur.		
  */ 
  static Real sync_f (Duration dur, Moment mom );

  /// Return exact duration, in midi-ticks if not-exact.
  static Duration ticks2_dur (int ticks_i );

  /// Return standardised duration, best guess if not exact.
  static Duration ticks2standardised_dur (int ticks_i );
};


#endif // DURATION_CONVERT_HH
