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
  static void add_rod (Score_element*me, Score_element * to, Real distance);
  static void add_spring (Score_element*me,Score_element * to, Real dist, Real strength);
  static void set_interface (Score_element*);
  static void remove_interface (Score_element*);
  static SCM get_minimum_distances (Score_element*);
  static SCM get_ideal_distances (Score_element*);
};

#endif /* SPACEABLE_ELEMENT_HH */

