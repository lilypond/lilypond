/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "grob.hh"


/*
  This special add_thing routine is slightly more efficient than

    set_prop (name,cons (thing, get_prop (name)))

  since it can reuse the handle returned by scm_assq().
*/

void
Group_interface::add_thing (Grob*me, SCM sym, SCM thing)
{
 SCM handle = scm_sloppy_assq (sym, me->mutable_property_alist_);
  if (handle != SCM_BOOL_F)
    {
      gh_set_cdr_x (handle, gh_cons (thing, gh_cdr (handle)));
      
    }
  else
    {
      /*
	There is no mutable prop yet, so create an entry, and put it in front of the
	mutable prop list.
      */
      handle = scm_sloppy_assq (sym, me->immutable_property_alist_);
      SCM tail = (handle != SCM_BOOL_F) ? gh_cdr(handle) : SCM_EOL;
      me->mutable_property_alist_ = gh_cons (gh_cons (sym, gh_cons (thing, tail)),
					     me->mutable_property_alist_);
    }
}

void
Group_interface::add_thing (Grob*me, String name, SCM thing)
{
  add_thing (me, ly_symbol2scm (name.ch_C()), thing);
}

int
Group_interface::count (Grob *me, String name)
{
  return scm_ilength (me->get_grob_property (name.ch_C ()));
}


void
Pointer_group_interface::add_grob (Grob*me, SCM name, Grob*p) 
{
  Group_interface::add_thing (me, name, p->self_scm ());
}
