/*   
  spring.hh -- declare Spring, Column_spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "drul-array.hh"

struct Column_spring {
  Paper_column *other_l_;
  Real distance_f_;

  /*
    TODO: make 2 strengths: one for stretching, and one for shrinking.
  */
  Real strength_f_;
  
  Column_spring ();
};

struct Spring{
  Drul_array<Item*> item_l_drul_;
  Real distance_f_;

  /*
    TODO: make 2 strengths: one for stretching, and one for shrinking.
  */
  Real strength_f_;
  void add_to_cols ();
  Spring ();
};


#endif /* SPRING_HH */

