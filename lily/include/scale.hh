/* 
  scale.hh -- declare Scale
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2007 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#ifndef SCALE_HH
#define SCALE_HH

#include "smobs.hh"
#include "rational.hh"
#include "std-vector.hh"

struct Scale
{
public:
  Scale (vector<Rational> const&);
  Scale (Scale const&);

  Rational tones_at_step (int step, int octave = 0) const;
  Rational step_size (int step) const;
  int step_count () const;
  int normalize_step (int step) const;

  DECLARE_SMOBS (Scale);

private:
  vector<Rational> step_tones_;
};

extern Scale *default_global_scale;

#endif /* SCALE_HH */


