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
#include "simple-closure.hh"

SCM
Grob::get_property_alist_chain (SCM def) const
{
  return scm_list_n (mutable_property_alist_,
		     immutable_property_alist_,
		     def,
		     SCM_UNDEFINED);
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
      if (!ly_is_procedure (v)
	  && !is_simple_closure (v)
	  && v != ly_symbol2scm ("calculation-in-progress") 
	  && !type_check_assignment (sym, v, ly_symbol2scm ("backend-type?")))
	abort ();
      check_interfaces_for_property (this, sym);
    }

  mutable_property_alist_ = scm_assq_set_x (mutable_property_alist_, sym, v);
}

//#define PROFILE_PROPERTY_ACCESSES
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
      SCM val = scm_cdr (handle);
      if (!ly_is_procedure (val)
	  && !is_simple_closure (val)
	  && !type_check_assignment (sym, val, 
				  ly_symbol2scm ("backend-type?")))
	abort ();

      check_interfaces_for_property (this, sym);
    }
  
  return (handle == SCM_BOOL_F) ? SCM_EOL : scm_cdr (handle);
}

SCM
Grob::internal_get_property (SCM sym) const
{
  SCM val = get_property_data (sym);
  if (ly_is_procedure (val)
      || is_simple_closure (val))
    {
      val = ((Grob*)this)->try_callback (sym, val);
    }
  
  return val;
}

#ifndef NDEBUG
#include "protected-scm.hh"

Protected_scm grob_property_callback_stack = SCM_EOL;
bool debug_property_callbacks = 0;
#endif

SCM
Grob::try_callback (SCM sym, SCM proc)
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

  SCM value = SCM_EOL;
  if (ly_is_procedure (proc))
    value = scm_call_1 (proc, self_scm ());
  else if (is_simple_closure (proc))
    {
      value = evaluate_with_simple_closure (self_scm (),
					    simple_closure_expression (proc));
    }
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

void
Grob::internal_set_object (SCM s, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  object_alist_ = scm_assq_set_x (object_alist_, s, v);
}

void
Grob::del_property (SCM sym)
{
  mutable_property_alist_ = scm_assq_remove_x (mutable_property_alist_, sym);
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

bool
Grob::is_live () const
{
  return immutable_property_alist_ != SCM_EOL;
}


bool
Grob::internal_has_interface (SCM k)
{
  return scm_c_memq (k, interfaces_) != SCM_BOOL_F;
}
