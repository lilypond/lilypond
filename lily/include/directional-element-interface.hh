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
  Score_element *elt_l_;
  
  Directional_element_interface (Score_element const *);
  void set  (Direction d);
  Direction get () const;
  bool has_interface_b () const;
  // bool set_interface ();
};


#endif /* DIRECTIONAL_ELEMENT_HH */

