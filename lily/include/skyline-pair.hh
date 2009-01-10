/* 
  skyline-pair.hh -- declare Skyline_pair
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2008--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#ifndef SKYLINE_PAIR_HH
#define SKYLINE_PAIR_HH

#include "skyline.hh"

class Skyline_pair
{
private:
  Drul_array<Skyline> skylines_;

  DECLARE_SIMPLE_SMOBS(Skyline_pair);
public:
  Skyline_pair ();
  Skyline_pair (vector<Box> const &boxes, Real horizon_padding, Axis a);
  Skyline_pair (Box const &, Real horizon_padding, Axis a);
  void raise (Real);
  void shift (Real);
  void insert (Box const &, Real horizon_padding, Axis);
  void merge (Skyline_pair const &other);
  Skyline &operator [] (Direction d);
  Skyline const &operator [] (Direction d) const;
  bool is_empty () const;
  void print () const;
  void print_points () const;
};

#endif /* SKYLINE_PAIR_HH */
