/*
  duration.hh -- declare Duration Plet

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

*/

// split into 4?

#ifndef DURATION_HH
#define DURATION_HH

#include "fproto.hh"
#include "moment.hh"
#include "plet.hh"
// ugh, to get me in lily lib
extern bool no_triplets_bo_g;


/**
  Class to handle "musical" durations. This means: balltype 1,2,4,etc. and dots.
   
  (dur)
  */
struct Duration {
  Duration ();
  /// is the "plet factor" of this note != 1 ?
  bool plet_b ();
  String str () const;
  void set_plet (int,int );
  void set_plet (Duration );
  static bool duration_type_b (int t);
  void set_ticks (int ticks_i );
  Moment length () const ;	// zo naai mij
  static int division_1_i_s;

  /// Logarithm of the base duration.
  int durlog_i_;
  int dots_i_;
  Plet plet_;
  int ticks_i_;
};
#endif // DURATION_HH

