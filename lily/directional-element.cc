/*   
  directional-element.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"


Directional_element_interface::Directional_element_interface (Score_element const *s)
{
  elt_l_ =  (Score_element*)s;
}

bool
Directional_element_interface::has_interface_b () const
{
  return isdir_b (elt_l_->get_elt_property ("direction"));
}


  

Direction
Directional_element_interface::get () const
{
  // return dir_;
  SCM d= elt_l_->get_elt_property ("direction");
  if (!isdir_b(d))
    return CENTER;
      
  return to_dir (d);
}

void
Directional_element_interface::set (Direction d) 
{
  elt_l_->set_elt_property ("direction", gh_int2scm (d));
}

Directional_element_interface
directional_element (Score_element const*s)
{
  return s;
}
