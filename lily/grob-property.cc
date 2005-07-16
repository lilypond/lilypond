/*
  Implement storage and manipulation of grob properties.
*/

#include <cstring>
#include <math.h>

#include "main.hh"
#include "input-smob.hh"
#include "pointer-group-interface.hh"
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

  set_prop (name, cons (thing, get_prop (name)))

  since it can reuse the handle returned by scm_assq ().
*/
// JUNKME.
void
Grob::add_to_list_property (SCM sym, SCM thing)
{
  SCM handle
    = scm_sloppy_assq (sym, mutable_property_alist_);

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
Grob::internal_set_property (SCM sym, SCM v)
{
#ifndef NDEBUG
  SCM grob_p = ly_lily_module_constant ("ly:grob?");
  SCM grob_list_p = ly_lily_module_constant ("grob-list?");
  SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
  
  if (type == grob_p
      || type == grob_list_p
      || (unsmob_grob (v) && ly_symbol2scm ("cause") != sym))
    {
      scm_display (scm_list_2 (sym, type), scm_current_output_port());
      assert (0);
    }
#endif

  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  if (do_internal_type_checking_global)
    {
      if (!type_check_assignment (sym, v, ly_symbol2scm ("backend-type?")))
	abort ();
      check_interfaces_for_property (this, sym);
    }

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, sym, v);
}

Protected_scm property_lookup_table;
LY_DEFINE(ly_property_lookup_stats, "ly:property-lookup-stats",
	  0,0,0, (),
	  "")
{
  return (SCM) property_lookup_table;
}


SCM
Grob::internal_get_property (SCM sym) const
{
#ifndef NDEBUG
  SCM grob_p = ly_lily_module_constant ("ly:grob?");
  SCM grob_list_p = ly_lily_module_constant ("grob-list?");
  SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
  
  if (type == grob_p
      || type == grob_list_p)
    {
      scm_display (scm_list_2 (sym, type), scm_current_output_port());
      assert (0);
    }
#endif

#if 0
  /*
    Statistics: which properties are looked up? 
  */
  if (scm_hash_table_p (property_lookup_table) != SCM_BOOL_T)
    {
      property_lookup_table = scm_c_make_hash_table (259);
    }

  SCM hashhandle = scm_hashq_get_handle (property_lookup_table, sym);
  if (hashhandle == SCM_BOOL_F)
    {
      scm_hashq_set_x (property_lookup_table, sym, scm_from_int (0));
      hashhandle = scm_hashq_get_handle (property_lookup_table, sym);
    }

  scm_set_cdr_x (hashhandle, scm_from_int (scm_to_int (scm_cdr (hashhandle)) + 1));
#endif
  
  
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
Grob::internal_set_object (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  object_alist_ = scm_assq_set_x (object_alist_, s, v);
}

SCM
Grob::internal_get_object (SCM sym) const
{
  SCM s = scm_sloppy_assq (sym, object_alist_);

  return (s == SCM_BOOL_F) ? SCM_EOL : scm_cdr (s);
}

void
Grob::substitute_object_links (SCM crit, SCM orig)
{
  set_break_subsititution (crit);
  object_alist_ = substitute_object_alist (orig, object_alist_);
}

bool
Grob::is_live () const
{
  return immutable_property_alist_ != SCM_EOL;
}
