/*   
  directional-element.hh -- declare Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "grob.hh"

struct Directional_element_interface 
{
public:
  static SCM direction_sym ;
  static void set  (Grob*,Direction d);
  static Direction get (Grob*) ;
  static bool has_interface (Grob*) ;
};


#endif /* DIRECTIONAL_ELEMENT_HH */

