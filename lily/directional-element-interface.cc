/*   
  directional-element-interface.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"

Direction
get_grob_direction (Grob*me) 
{
  SCM d = me->get_property ("direction");
  if (!is_direction (d))
    return CENTER;
      
  return to_dir (d);
}

void
set_grob_direction (Grob*me, Direction d) 
{
  SCM sd = scm_int2num (d);
  me->set_property ("direction", sd);
}
