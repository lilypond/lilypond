/*
  Implement storage and manipulation of grob properties.
 */

#include <string.h>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"
#include "group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "grob.hh"
#include "spanner.hh"
#include "item.hh"
#include "misc.hh"
#include "item.hh"


SCM
Grob::get_property_alist_chain (SCM def) const
{
  return  scm_list_n (mutable_property_alist_,
		      immutable_property_alist_,
		      def,
		      SCM_UNDEFINED);
}



/*
  This special add_thing routine is slightly more efficient than

    set_prop (name,cons (thing, get_prop (name)))

  since it can reuse the handle returned by scm_assq().
*/
void
Grob::add_to_list_property (SCM sym, SCM thing) 
{
  SCM handle
    = scm_sloppy_assq (sym, mutable_property_alist_)
    ;

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
      handle = scm_sloppy_assq (sym, immutable_property_alist_);
      SCM tail = (handle != SCM_BOOL_F) ? gh_cdr(handle) : SCM_EOL;
      SCM val = gh_cons (thing, tail);

      mutable_property_alist_ = gh_cons (gh_cons (sym, val),
					 mutable_property_alist_);
    }
}


extern void check_interfaces_for_property (Grob const *me, SCM sym);

void
Grob::internal_set_grob_property (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!live ())
    return;

  if (internal_type_checking_global_b)
    {
      if (!type_check_assignment (s, v, ly_symbol2scm ("backend-type?")))
	abort ();
      check_interfaces_for_property (this, s);
    }

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, s, v);
}


SCM
Grob::internal_get_grob_property (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, mutable_property_alist_);
  if (s != SCM_BOOL_F)
    return ly_cdr (s);

  s = scm_sloppy_assq (sym, immutable_property_alist_);
  
  if (internal_type_checking_global_b && gh_pair_p (s))
    {
      if (!type_check_assignment (sym, gh_cdr (s),
				  ly_symbol2scm ("backend-type?")))
	abort ();
      check_interfaces_for_property (this, sym);
    }

  return (s == SCM_BOOL_F) ? SCM_EOL : ly_cdr (s); 
}

void
Grob::substitute_mutable_properties (SCM crit, SCM orig)
{
  set_break_subsititution(crit);
  mutable_property_alist_ = substitute_mutable_property_alist (orig);
}


bool
Grob::live () const
{
  return immutable_property_alist_ != SCM_EOL;
}
