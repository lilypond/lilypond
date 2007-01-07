/* 
  spacing-options.hh -- declare Spacing_options
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#ifndef SPACING_OPTIONS_HH
#define SPACING_OPTIONS_HH

#include "lily-proto.hh"
#include "rational.hh"
#include "std-vector.hh"

struct Spacing_options
{
  bool packed_;
  bool stretch_uniformly_;
  bool float_nonmusical_columns_;
  bool float_grace_columns_;
  Rational global_shortest_;
  Real increment_;
  Real shortest_duration_space_;
  vector<Grob*> columns_;
  
  Spacing_options();
  void init_from_grob (Grob *me);
  Real get_duration_space (Rational d, bool *) const;
};
#endif /* SPACING_OPTIONS_HH */
