/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "score-element.hh"


void
Group_interface::add_thing (Score_element*me, String name, SCM s)
{
  me->set_elt_property (name.ch_C (),
 			    gh_cons (s, me->get_elt_property (name.ch_C())));
}


int
Group_interface::count (Score_element *me, String name)
{
  return scm_ilength (me->get_elt_property (name.ch_C()));
}


void
Pointer_group_interface::add_element (Score_element*me, String name, Score_element*p) 
{
  Group_interface::add_thing (me, name, p->self_scm());
}
