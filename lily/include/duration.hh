/*
  duration.hh -- declare Duration
  
  source file of the LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#ifndef DURATION_HH
#define DURATION_HH

#include "flower-proto.hh"
#include "rational.hh"


/**
   A musical duration.
  */
struct Duration {
  Duration ();
  /// is the "plet factor" of this note != 1 ?
  bool plet_b ();
  String str () const;
  void set_plet (int,int );
  void compress (Rational);
  Rational length_mom () const ;

  /// Logarithm of the base duration.
  int durlog_i_;
  int dots_i_;

  /*
    JUNKME.
   */
  int tuplet_iso_i_;  // 2/3; 2 is not duration, maar of count!
  int tuplet_type_i_; 

};
#endif // DURATION_HH

