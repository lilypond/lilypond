/*
  Implement storage and manipulation of grob properties.
*/

#include <cstring>

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
#include "program-option.hh"
#include "profile.hh"

SCM
Grob::get_property_alist_chain (SCM def) const
{
  return scm_list_n (mutable_property_alist_,
		     immutable_property_alist_,
		     def,
		     SCM_UNDEFINED);
}

SCM
Grob::get_interfaces () const
{
  return interfaces_;
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
    scm_set_cdr_x (handle, scm_cons (thing, scm_cdr (handle)));
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
      scm_display (scm_list_2 (sym, type), scm_current_output_port ());
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

//#define PROFILE_PROPERTY_ACCESSES

/*
  Ugh C&P Coding.

  Retrieve property without triggering callback.
 */
SCM
Grob::get_property_data (SCM sym) const
{
#ifndef NDEBUG
  if (profile_property_accesses)
    note_property_access (&grob_property_lookup_table, sym);
#endif
  
  SCM handle = scm_sloppy_assq (sym, mutable_property_alist_);
  if (handle != SCM_BOOL_F)
    return scm_cdr (handle);

  handle = scm_sloppy_assq (sym, immutable_property_alist_);

  if (do_internal_type_checking_global && scm_is_pair (handle))
    {
      if (!type_check_assignment (sym, scm_cdr (handle),
				  ly_symbol2scm ("backend-type?")))
	abort ();

      check_interfaces_for_property (this, sym);
    }
  
  return (handle == SCM_BOOL_F) ? SCM_EOL : scm_cdr (handle);
}

SCM
Grob::internal_get_property (SCM sym) const
{
#ifndef NDEBUG
  if (profile_property_accesses)
    note_property_access (&grob_property_lookup_table, sym);
#endif
  
  SCM handle = scm_sloppy_assq (sym, mutable_property_alist_);
  if (handle != SCM_BOOL_F)
    return scm_cdr (handle);

  handle = scm_sloppy_assq (sym, immutable_property_alist_);

  if (do_internal_type_checking_global && scm_is_pair (handle))
    {
      if (!type_check_assignment (sym, scm_cdr (handle),
				  ly_symbol2scm ("backend-type?")))
	abort ();

      check_interfaces_for_property (this, sym);
    }

  
  if (handle == SCM_BOOL_F)
    {
      SCM value = ((Grob*)  this)->try_callback (sym);
      if (value != SCM_UNSPECIFIED)
	return value;
    }
  
  return (handle == SCM_BOOL_F) ? SCM_EOL : scm_cdr (handle);
}

#ifndef NDEBUG
#include "protected-scm.hh"
Protected_scm grob_property_callback_stack = SCM_EOL;
bool debug_property_callbacks = 1;
#endif

SCM
Grob::try_callback (SCM sym)
{      
  SCM handle = scm_sloppy_assq (sym, property_callbacks_);
  if (scm_is_pair (handle))
    {
      SCM proc = scm_cdr (handle);
      if (ly_is_procedure (proc))
	{
	  SCM marker = ly_symbol2scm ("calculation-in-progress");
	  /*
	    need to put a value in SYM to ensure that we don't get a
	    cyclic call chain.
	  */
	  mutable_property_alist_
	    = scm_assq_set_x (mutable_property_alist_, sym, marker);

#ifndef NDEBUG
	  if (debug_property_callbacks)
	    grob_property_callback_stack = scm_acons (sym, proc, grob_property_callback_stack);
#endif
	  SCM value = scm_call_1 (proc, self_scm ());
#ifndef NDEBUG
	  if (debug_property_callbacks)
	    grob_property_callback_stack = scm_cdr (grob_property_callback_stack);
#endif
	  
	  /*
	    If the function returns SCM_UNSPECIFIED, we assume the
	    property has been set with an explicit set_property()
	    call.
	   */
	  if (value == SCM_UNSPECIFIED)
	    {
	      value = internal_get_property (sym);
	      if (value == marker)
		mutable_property_alist_ = scm_assq_remove_x (mutable_property_alist_, marker);
	    }
	  else
	    internal_set_property (sym, value);
	  
	  return value;
	}
      else
	programming_error ("Callback should be procedure type");
    }

  return SCM_UNSPECIFIED;
}

void
Grob::internal_set_object (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  object_alist_ = scm_assq_set_x (object_alist_, s, v);
}

void
Grob::set_callback (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  /*
    property_callbacks_ is r/o in principle, so we tack it in front.
   */ 
  property_callbacks_ = scm_acons (s,v, property_callbacks_);
}



SCM
Grob::internal_get_object (SCM sym) const
{
#ifdef PROFILE_PROPERTY_ACCESSES
  note_property_access (&grob_property_lookup_table, sym);
#endif

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
