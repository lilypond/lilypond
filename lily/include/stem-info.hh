/*
  stem-info.hh -- declare Stem_info

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"

struct Stem_info {
  Real x;
  int dir_;
  Real idealy_f_;
  Real miny_f_;
  int beams_i_;

  Stem_info();
  Stem_info (Stem const *);
};

#endif // STEM_INFO_HH
