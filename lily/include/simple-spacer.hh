/*   
  simple-spacer.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SIMPLE_SPACER_HH
#define SIMPLE_SPACER_HH

#include "real.hh"
#include "array.hh"
#include "lily-proto.hh"
#include "list.hh"
#include "drul-array.hh"

struct Rod_info {
  Real distance_f_;
  Drul_array<int> cols_;
};

struct Spring_info {
  /// the ideal distance
  Real space_f_;

  /// Hooke's constant: how strong are the "springs" attached to columns
  Real hooke_f_;

  Real blocking_stretch_f_;
  Rod_info * blocking_rod_l_;
  void set (Idealspacing *);
  Spring_info();
};


class Simple_spring_spacer {
  Array<Spring_info> springs_;
  Pointer_list<Rod_info*> rods_;

  void init ();
  Array<Real> solve ();
  
};

#endif /* SIMPLE_SPACER_HH */

