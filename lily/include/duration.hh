/*
  duration.hh -- declare Duration
  
  source file of the LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#ifndef DURATION_HH
#define DURATION_HH

#include "flower-proto.hh"
#include "moment.hh"
#include "smobs.hh"

/**
   A musical duration.
  */
struct Duration {
  Duration ();
  Duration (int, int);
  String str () const;
  void set_plet (int,int );
  Duration compressed (Rational) const;
  Rational length_mom () const ;
  static int compare (Duration const&, Duration const&);

  SCM smobbed_copy () const;
  DECLARE_SCHEME_CALLBACK (less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS (Duration,);

public:
  int duration_log ()const;
  int dot_count () const;
  
private:
    /// Logarithm of the base duration.
  int durlog_i_;
  int dots_i_;

  Moment factor_;

};

#include "compare.hh"
INSTANTIATE_COMPARE (Duration, Duration::compare);
Duration*unsmob_duration (SCM);
// int compare (Array<Duration>*, Array<Duration>*);

#endif // DURATION_HH

