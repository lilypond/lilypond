/*
  Implement storage and manipulation of grob properties.
 */

#include <cstring>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"
#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "output-def.hh"
#include "spanner.hh"
#include "item.hh"
#include "misc.hh"
#include "item.hh"


SCM
Grob::get_property_alist_chain (SCM def) const
{
  return scm_list_n (mutable_property_alist_,
		     immutable_property_alist_,
		     def,
		     SCM_UNDEFINED);
}



/*
  This special add_thing routine is slightly more efficient than

    set_prop (name,cons (thing, get_prop (name)))

  since it can reuse the handle returned by scm_assq ().
*/
void
Grob::add_to_list_property (SCM sym, SCM thing) 
{
  SCM handle
    = scm_sloppy_assq (sym, mutable_property_alist_)
    ;

  if (handle != SCM_BOOL_F)
    {
      scm_set_cdr_x (handle, scm_cons (thing, scm_cdr (handle)));
    }
  else
    {
      /*
	There is no mutable prop yet, so create an entry, and put it in front of the
	mutable prop list.
      */
      handle = scm_sloppy_assq (sym, immutable_property_alist_);
      SCM tail = (handle != SCM_BOOL_F) ? scm_cdr (handle) : SCM_EOL;
      SCM val = scm_cons (thing, tail);

      mutable_property_alist_ = scm_cons (scm_cons (sym, val),
					 mutable_property_alist_);
    }
}


extern void check_interfaces_for_property (Grob const *me, SCM sym);

void
Grob::internal_set_property (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  if (do_internal_type_checking_global)
    {
      if (!type_check_assignment (s, v, ly_symbol2scm ("backend-type?")))
	abort ();
      check_interfaces_for_property (this, s);
    }

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}


SCM
Grob::internal_get_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return scm_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  
  if (do_internal_type_checking_global && scm_is_pair (s))
    {
      if (!type_check_assignment (sym, scm_cdr (s),
				  ly_symbol2scm ("backend-type?")))
	abort ();

      check_interfaces_for_property (this, sym);
    }

  return (s == SCM_BOOL_F) ? SCM_EOL : scm_cdr (s); 
}

void
Grob::substitute_mutable_properties (SCM crit, SCM orig)
{
  set_break_subsititution (crit);
  mutable_property_alist_ = substitute_mutable_property_alist (orig);
}


bool
Grob::is_live () const
{
  return immutable_property_alist_ != SCM_EOL;
}
