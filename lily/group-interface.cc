/*   
  group-interface.cc -- implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "group-interface.hh"
#include "item.hh"

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



Link_array<Grob>
extract_grob_array (Grob const *elt, SCM symbol)
{
  Link_array<Grob> arr;

  for (SCM s = elt->internal_get_property (symbol); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM e = scm_car (s);
      arr.push (unsmob_grob (e));
    }

  arr.reverse ();
  return arr;
}


Link_array<Item>
extract_item_array (Grob const *elt, SCM symbol)
{
  Link_array<Item> arr;
  for (SCM s = elt->internal_get_property (symbol); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM e = scm_car (s);
      arr.push (dynamic_cast<Item*> (unsmob_grob (e)));
    }

  arr.reverse ();
  return arr;
}
