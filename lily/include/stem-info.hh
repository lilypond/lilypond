/*
  stem-info.hh -- declare Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef STEM_INFO_HH
#define STEM_INFO_HH

#include "real.hh"

struct Stem_info {
  Real x_;
  Direction dir_;
  void set_direction (Direction d ) { dir_ =  d; }
  Direction get_direction () const { return dir_; }

  int beam_dir_;
  Real idealy_f_;
  Real miny_f_;
  Real maxy_f_;
  int mult_i_;
  Real interstaff_f_;
  Stem* stem_l_;

  Stem_info ();
  Stem_info (Stem *, int);
};

#endif // STEM_INFO_HH
