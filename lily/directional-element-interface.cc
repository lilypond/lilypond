/*   
  directional-element.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"




bool
Directional_element_interface::has_interface (Grob*me) 
{
  return isdir_b (me->get_grob_property ("direction"));
}

Direction
Directional_element_interface::get (Grob*me) 
{
  // return dir_;
  SCM d= me->get_grob_property ("direction");
  if (!isdir_b (d))
    return CENTER;
      
  return to_dir (d);
}

void
Directional_element_interface::set (Grob*me, Direction d) 
{
  SCM sd = gh_int2scm (d);

  /*
    Vain attempt to save some conses.
   */
  if (me->get_grob_property ("direction") != sd)
    me->set_grob_property ("direction", sd);
}
