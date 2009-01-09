/*
  duration.hh -- declare Duration

  source file of the LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef DURATION_HH
#define DURATION_HH

#include "moment.hh"

/**
   A musical duration.
*/
struct Duration
{
public:

  Duration ();
  Duration (int, int);
  Duration (Rational, bool scale);
  string to_string () const;

  Duration compressed (Rational) const;
  Rational get_length () const;
  Rational factor () const { return factor_; }
  int duration_log () const;
  int dot_count () const;

  static int compare (Duration const &, Duration const &);

  DECLARE_SCHEME_CALLBACK (less_p, (SCM a, SCM b));
  DECLARE_SIMPLE_SMOBS (Duration);

private:
  /// Logarithm of the base duration.
  int durlog_;
  int dots_;

  Rational factor_;
};

INSTANTIATE_COMPARE (Duration, Duration::compare);
DECLARE_UNSMOB (Duration, duration);

#endif // DURATION_HH

