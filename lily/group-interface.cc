/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "grob.hh"

void
Group_interface::add_thing (Grob*me, SCM sym, SCM thing)
{
  me->add_to_list_property (sym, thing);
}


void
Group_interface::add_thing (Grob*me, String name, SCM thing)
{
  add_thing (me, ly_symbol2scm (name.to_str0 ()), thing);
}

int
Group_interface::count (Grob *me, String name)
{
  return scm_ilength (me->get_grob_property (name.to_str0 ()));
}


void
Pointer_group_interface::add_grob (Grob*me, SCM name, Grob*p) 
{
  Group_interface::add_thing (me, name, p->self_scm ());
}
