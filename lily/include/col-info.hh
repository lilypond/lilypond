/*
  col-info.hh -- declare Column_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COL_INFO_HH
#define COL_INFO_HH

#include "lily-proto.hh"
#include "pointer.hh"
#include "interval.hh"
#include "drul-array.hh"

struct Spacer_rod {
  Real distance_f_;
  int other_idx_;
  void print () const;
};


/// helper struct for #Spacing_problem#
struct Column_info {
  Paper_column *pcol_l_;
  P<Real> fixpos_p_;

  Interval width_;
  int rank_i_;
  /// did some tricks to make this column come out.
  bool ugh_b_;		

  Drul_array< Array<Spacer_rod> > rods_;
  
  Column_info();
  Column_info (Paper_column *,Real const *);

  int rank_i () const;
  void print() const;
  bool fixed_b() const ;
  Real fixed_position() const ;
};

#endif // COL_INFO_HH
