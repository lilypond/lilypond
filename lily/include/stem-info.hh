/*
  stem-info.hh -- declare Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"

/*
  Parameters for a stem, (multiply with stemdirection, to get real values
  for a downstem.)
  
 */
struct Stem_info 
{
  Real ideal_y;

  // fixme: we're an Interval!
  Real min_y;
  Real max_y;
};

#endif // STEM_INFO_HH
