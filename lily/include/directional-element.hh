/*   
  directional-element.hh -- declare Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef DIRECTIONAL_ELEMENT_HH
#define DIRECTIONAL_ELEMENT_HH

#include "score-element.hh"

struct Directional_element : public virtual Score_element
{
  void set_direction (Direction d);
  Direction get_direction () const;

  VIRTUAL_COPY_CONS(Score_element);
  Directional_element ();
};

#endif /* DIRECTIONAL_ELEMENT_HH */

