/*   
  directional-element.hh -- declare Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "grob.hh"

void set_grob_direction (Grob*, Direction);
Direction get_grob_direction (Grob*);

#endif /* DIRECTIONAL_ELEMENT_HH */

