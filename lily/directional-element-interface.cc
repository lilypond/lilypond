/*   
  directional-element-interface.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"


Direction
get_grob_direction (Grob*me) 
{
  SCM d= me->get_grob_property ("direction");
  if (!is_direction (d))
    return CENTER;
      
  return to_dir (d);
}

void
set_grob_direction (Grob*me, Direction d) 
{
  SCM sd = gh_int2scm (d);
  me->set_grob_property ("direction", sd);
}
