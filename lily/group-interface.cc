/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "grob.hh"


void
Group_interface::add_thing (Grob*me, String name, SCM s)
{
  me->set_grob_property (name.ch_C (),
 			    gh_cons (s, me->get_grob_property (name.ch_C ())));
}


int
Group_interface::count (Grob *me, String name)
{
  return scm_ilength (me->get_grob_property (name.ch_C ()));
}


void
Pointer_group_interface::add_element (Grob*me, String name, Grob*p) 
{
  Group_interface::add_thing (me, name, p->self_scm ());
}
