/*   
  directional-element.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element.hh"


Directional_element::Directional_element ()
{
}

Direction
Directional_element::get_direction () const
{
  // return dir_;
  SCM d= get_elt_property ("direction");
  if (!isdir_b(d))
    return CENTER;
      
  return to_dir (d);
}

void
Directional_element::set_direction (Direction d) 
{
  set_elt_property ("direction", gh_int2scm (d));
}
