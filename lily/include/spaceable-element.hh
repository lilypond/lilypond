/*   
  spaceable-element.hh -- declare Spaceable_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACEABLE_ELEMENT_HH
#define SPACEABLE_ELEMENT_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

struct Spaceable_element
{
  /// set a minimum distance
  static void add_rod (Grob*me, Grob * to, Real distance);
  static void add_spring (Grob*me,Grob * to, Real dist, Real strength);
  static void set_interface (Grob*);
  static void remove_interface (Grob*);
  static SCM get_minimum_distances (Grob*);
  static SCM get_ideal_distances (Grob*);
};

#endif /* SPACEABLE_ELEMENT_HH */

