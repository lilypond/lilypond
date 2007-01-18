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
  vector<Rational> step_tones_;
  Scale ();
  Scale (Scale const&);
  DECLARE_SMOBS (Scale);
};

extern Scale *default_global_scale;

#endif /* SCALE_HH */


