/*   
  group-interface.cc -- implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "group-interface.hh"

void
Group_interface::add_thing (Grob*me, SCM sym, SCM thing)
{
  me->add_to_list_property (sym, thing);
}


int
Group_interface::count (Grob *me, SCM sym)
{
  return scm_ilength (me->internal_get_property (sym));
}


void
Pointer_group_interface::add_grob (Grob*me, SCM name, Grob*p) 
{
  Group_interface::add_thing (me, name, p->self_scm ());
}
