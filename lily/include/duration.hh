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
  static int compare (Duration const&, Duration const&);

  /// Logarithm of the base duration.
  int durlog_i_;
  int dots_i_;

  /*
    JUNKME.
   */
  int tuplet_iso_i_;  // 2/3; 2 is not duration, maar of count!
  int tuplet_type_i_; 

};

#include "compare.hh"
INSTANTIATE_COMPARE(Duration, Duration::compare);

int compare (Array<Duration>*, Array<Duration>*);

#endif // DURATION_HH

