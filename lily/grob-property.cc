/*
  Implement storage and manipulation of grob properties.
*/

#include <cstring>

#include "main.hh"
#include "input.hh"
#include "pointer-group-interface.hh"
#include "misc.hh"
#include "paper-score.hh"
#include "output-def.hh"
#include "spanner.hh"
#include "international.hh"
#include "item.hh"
#include "program-option.hh"
#include "profile.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"
#include "protected-scm.hh"

Protected_scm grob_property_callback_stack (SCM_EOL);

extern bool debug_property_callbacks;

#ifdef DEBUG
static void
print_property_callback_stack ()
{
  int frame = 0;
  for (SCM s = grob_property_callback_stack; scm_is_pair (s); s = scm_cdr (s))
    message (_f ("%d: %s", frame++, ly_scm_write_string (scm_car (s)).c_str ()));
}
#endif

static Protected_scm modification_callback (SCM_EOL);
static Protected_scm cache_callback (SCM_EOL);

/*
FIXME: this should use ly:set-option interface instead.
*/

LY_DEFINE (ly_set_grob_modification_callback, "ly:set-grob-modification-callback",
           1, 0, 0, (SCM cb),
           "Specify a procedure that will be called every time LilyPond"
           " modifies a grob property.  The callback will receive as"
           " arguments the grob that is being modified, the name of the"
           " C++ file in which the modification was requested, the line"
           " number in the C++ file in which the modification was requested,"
           " the name of the function in which the modification was"
           " requested, the property to be changed, and the new value for"
           " the property.")
{
  modification_callback = (ly_is_procedure (cb)) ? cb : SCM_BOOL_F;
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_set_property_cache_callback, "ly:set-property-cache-callback",
           1, 0, 0, (SCM cb),
           "Specify a procedure that will be called whenever lilypond"
           " calculates a callback function and caches the result.  The"
           " callback will receive as arguments the grob whose property it"
           " is, the name of the property, the name of the callback that"
           " calculated the property, and the new (cached) value of the"
           " property.")
{
  cache_callback = (ly_is_procedure (cb)) ? cb : SCM_BOOL_F;
  return SCM_UNSPECIFIED;
}

void
Grob::instrumented_set_property (SCM sym, SCM v,
                                 char const *file,
                                 int line,
                                 char const *fun)
{
#ifdef DEBUG
  if (ly_is_procedure (modification_callback))
    scm_apply_0 (modification_callback,
                 scm_list_n (self_scm (),
                             scm_from_locale_string (file),
                             scm_from_int (line),
                             scm_from_ascii_string (fun),
                             sym, v, SCM_UNDEFINED));
#else
  (void) file;
  (void) line;
  (void) fun;
#endif

  internal_set_property (sym, v);
}

SCM
Grob::get_property_alist_chain (SCM def) const
{
  return scm_list_3 (mutable_property_alist_,
                     immutable_property_alist_,
                     def);
}

extern void check_interfaces_for_property (Grob const *me, SCM sym);

void
Grob::internal_set_property (SCM sym, SCM v)
{
  internal_set_value_on_alist (&mutable_property_alist_,
                               sym, v);

}

void
Grob::internal_set_value_on_alist (SCM *alist, SCM sym, SCM v)
{
  /* Perhaps we simply do the assq_set, but what the heck. */
  if (!is_live ())
    return;

  if (do_internal_type_checking_global)
    {
      if (!ly_is_procedure (v)
          && !unsmob<Unpure_pure_container> (v)
          && !scm_is_eq (v, ly_symbol2scm ("calculation-in-progress")))
        type_check_assignment (sym, v, ly_symbol2scm ("backend-type?"));

      check_interfaces_for_property (this, sym);
    }

  *alist = scm_assq_set_x (*alist, sym, v);
}

SCM
Grob::internal_get_property_data (SCM sym) const
{
#ifdef DEBUG
  if (profile_property_accesses)
    note_property_access (&grob_property_lookup_table, sym);
#endif

  SCM handle = scm_sloppy_assq (sym, mutable_property_alist_);
  if (scm_is_true (handle))
    return scm_cdr (handle);

  handle = scm_sloppy_assq (sym, immutable_property_alist_);

  if (do_internal_type_checking_global && scm_is_pair (handle))
    {
      SCM val = scm_cdr (handle);
      if (!ly_is_procedure (val)
          && !unsmob<Unpure_pure_container> (val))
        type_check_assignment (sym, val, ly_symbol2scm ("backend-type?"));

      check_interfaces_for_property (this, sym);
    }

  return scm_is_false (handle) ? SCM_EOL : scm_cdr (handle);
}

SCM
Grob::internal_get_property (SCM sym) const
{
  SCM val = get_property_data (sym);

#ifdef DEBUG
  if (scm_is_eq (val, ly_symbol2scm ("calculation-in-progress")))
    {
      programming_error (to_string ("cyclic dependency: calculation-in-progress encountered for #'%s (%s)",
                                    ly_symbol2string (sym).c_str (),
                                    name ().c_str ()));//assert (1==0);
      if (debug_property_callbacks)
        {
          message ("backtrace: ");
          print_property_callback_stack ();
        }
    }
#endif

  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (val))
    val = upc->unpure_part ();

  if (ly_is_procedure (val))
    {
      Grob *me = ((Grob *)this);
      val = me->try_callback_on_alist (&me->mutable_property_alist_, sym, val);
    }

  return val;
}

/* Unlike internal_get_property, this function does no caching. Use it, therefore, with caution. */
SCM
Grob::internal_get_pure_property (SCM sym, int start, int end) const
{
  SCM val = internal_get_property_data (sym);
  if (ly_is_procedure (val))
    return call_pure_function (val, scm_list_1 (self_scm ()), start, end);

  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (val)) {
    // Do cache, if the function ignores 'start' and 'end'
    if (upc->is_unchanging ())
      return internal_get_property (sym);
    else
      return call_pure_function (val, scm_list_1 (self_scm ()), start, end);
  }

  return val;
}

SCM
Grob::internal_get_maybe_pure_property (SCM sym, bool pure, int start, int end) const
{
  return pure ? internal_get_pure_property (sym, start, end) : internal_get_property (sym);
}

SCM
Grob::try_callback_on_alist (SCM *alist, SCM sym, SCM proc)
{
  SCM marker = ly_symbol2scm ("calculation-in-progress");
  /*
    need to put a value in SYM to ensure that we don't get a
    cyclic call chain.
  */
  *alist = scm_assq_set_x (*alist, sym, marker);

#ifdef DEBUG
  if (debug_property_callbacks)
    grob_property_callback_stack = scm_cons (scm_list_3 (self_scm (), sym, proc), grob_property_callback_stack);
#endif

  SCM value = SCM_EOL;
  if (ly_is_procedure (proc))
    value = scm_call_1 (proc, self_scm ());

#ifdef DEBUG
  if (debug_property_callbacks)
    grob_property_callback_stack = scm_cdr (grob_property_callback_stack);
#endif

  if (scm_is_eq (value, SCM_UNSPECIFIED))
    {
      value = get_property_data (sym);
      assert (scm_is_null (value) || scm_is_eq (value, marker));
      if (scm_is_eq (value, marker))
        *alist = scm_assq_remove_x (*alist, sym);
    }
  else
    {
#ifdef DEBUG
      if (ly_is_procedure (cache_callback))
        scm_call_4 (cache_callback,
                    self_scm (),
                    sym,
                    proc,
                    value);
#endif
      internal_set_value_on_alist (alist, sym, value);
    }

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
Grob::internal_del_property (SCM sym)
{
  mutable_property_alist_ = scm_assq_remove_x (mutable_property_alist_, sym);
}

SCM
Grob::internal_get_object (SCM sym) const
{
  if (profile_property_accesses)
    note_property_access (&grob_property_lookup_table, sym);

  SCM s = scm_sloppy_assq (sym, object_alist_);

  if (scm_is_true (s))
    {
      SCM val = scm_cdr (s);
      if (ly_is_procedure (val)
          || unsmob<Unpure_pure_container> (val))
        {
          Grob *me = ((Grob *)this);
          val = me->try_callback_on_alist (&me->object_alist_, sym, val);
        }

      return val;
    }

  return SCM_EOL;
}

bool
Grob::is_live () const
{
  return scm_is_pair (immutable_property_alist_);
}

bool
Grob::internal_has_interface (SCM k)
{
  return scm_is_true (scm_c_memq (k, interfaces_));
}

SCM
call_pure_function (SCM value, SCM args, int start, int end)
{
  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (value))
    {
      if (upc->is_unchanging ())
        {
          // Don't bother forming an Unpure_pure_call here.
          value = upc->unpure_part ();

          if (ly_is_procedure (value))
            return scm_apply_0 (value, args);
          return value;
        }

      value = upc->pure_part ();

      if (ly_is_procedure (value))
        return scm_apply_3 (value,
                            scm_car (args),
                            scm_from_int (start),
                            scm_from_int (end),
                            scm_cdr (args));

      return value;
    }

  if (!ly_is_procedure (value))
    return value;

  return SCM_BOOL_F;
}
