/*
  stem-info.hh -- declare Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"

struct Stem_info 
{
  Real idealy_f_;

  // fixme: we're an Interval!
  Real miny_f_;
  Real maxy_f_;
};

#endif // STEM_INFO_HH
