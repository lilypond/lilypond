/*
  stem-info.hh -- declare Stem_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"
#include "direction.hh"

/*
  Parameters for a stem, (multiply with stemdirection, to get real values
  for a downstem.)
*/
struct Stem_info
{
  Direction dir_;
  Real ideal_y_;
  Real shortest_y_;
  Stem_info ();
  void scale (Real);
};

#endif // STEM_INFO_HH
