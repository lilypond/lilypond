/*   
  directional-element.hh -- declare Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "score-element.hh"

struct Directional_element_interface 
{
public:
  static SCM direction_sym ;
  static void set  (Score_element*,Direction d);
  static Direction get (Score_element*) ;
  static bool has_interface (Score_element*) ;
};


#endif /* DIRECTIONAL_ELEMENT_HH */

