/*   
  directional-element.cc -- implement Directional_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "directional-element-interface.hh"


SCM Directional_element_interface::direction_sym;

static void
init_functions ()
{
  Directional_element_interface::direction_sym = scm_permanent_object (ly_symbol2scm ("direction"));
}
ADD_SCM_INIT_FUNC(Directional, init_functions);


bool
Directional_element_interface::has_interface (Grob*me) 
{
  return isdir_b (me->get_grob_property (direction_sym));
}

Direction
Directional_element_interface::get (Grob*me) 
{
  // return dir_;
  SCM d= me->get_grob_property (direction_sym);
  if (!isdir_b(d))
    return CENTER;
      
  return to_dir (d);
}

void
Directional_element_interface::set (Grob*me, Direction d) 
{
  SCM sd = gh_int2scm (d);

  if (me->get_grob_property (direction_sym) != sd)
    me->set_grob_property (direction_sym, sd);
}
