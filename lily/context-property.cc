/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "context.hh"
#include "engraver.hh"
#include "global-context.hh"
#include "grob-properties.hh"
#include "international.hh"
#include "item.hh"
#include "main.hh"
#include "simple-closure.hh"
#include "smobs.hh"
#include "spanner.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"

/*
  like execute_general_pushpop_property(), but typecheck
  grob_property_path and context_property.
*/
void
general_pushpop_property (Context *context,
                          SCM context_property,
                          SCM grob_property_path,
                          SCM new_value)
{
  if (!scm_is_symbol (context_property)
      || !scm_is_symbol (scm_car (grob_property_path)))
    {
      warning (_ ("need symbol arguments for \\override and \\revert"));
      if (do_internal_type_checking_global)
        assert (false);
    }

  Grob_property_info (context, context_property).pushpop
    (grob_property_path, new_value);
}

bool
typecheck_grob (SCM symbol, SCM value)
{
  if (is_unpure_pure_container (value))
    return typecheck_grob (symbol, unpure_pure_container_unpure_part (value))
      && typecheck_grob (symbol, unpure_pure_container_pure_part (value));
  return ly_is_procedure (value)
    || is_simple_closure (value)
    || type_check_assignment (symbol, value, ly_symbol2scm ("backend-type?"));
}

class Grob_properties {
  friend class Grob_property_info;
  friend SCM ly_make_grob_properties (SCM);
  SCM alist_;
  SCM based_on_;

  Grob_properties (SCM alist, SCM based_on) :
    alist_ (alist), based_on_ (based_on) { }
  DECLARE_SIMPLE_SMOBS (Grob_properties);
};

#include "ly-smobs.icc"
IMPLEMENT_SIMPLE_SMOBS (Grob_properties);
IMPLEMENT_DEFAULT_EQUAL_P (Grob_properties);
IMPLEMENT_TYPE_P (Grob_properties, "ly:grob-properties?");

SCM
Grob_properties::mark_smob (SCM smob)
{
  Grob_properties *gp = (Grob_properties *) SCM_SMOB_DATA (smob);
  scm_gc_mark (gp->alist_);
  return gp->based_on_;
}

int
Grob_properties::print_smob (SCM /*smob*/, SCM port, scm_print_state *)
{
  scm_puts ("#<Grob_properties>", port);

  return 1;
}

LY_DEFINE (ly_make_grob_properties, "ly:make-grob-properties",
           1, 0, 0, (SCM alist),
           "This packages the given property list @var{alist} in"
           " a grob property container stored in a context property"
           " with the name of a grob.")
{
  LY_ASSERT_TYPE (ly_is_list, alist, 1);
  return Grob_properties (alist, SCM_EOL).smobbed_copy ();
}


Grob_property_info
Grob_property_info::find ()
{
  if (props_)
    return *this;
  SCM res = SCM_UNDEFINED;
  if (Context *c = context_->where_defined (symbol_, &res))
    if (c != context_)
      return Grob_property_info (c, symbol_, Grob_properties::unsmob (res));
  props_  = Grob_properties::unsmob (res);
  return *this;
}

bool
Grob_property_info::check ()
{
  if (props_)
    return true;
  SCM res = SCM_UNDEFINED;
  if (context_->here_defined (symbol_, &res))
    props_ = Grob_properties::unsmob (res);
  return props_;
}

bool
Grob_property_info::create ()
{
  // Using scm_hashq_create_handle_x would seem like the one-lookup
  // way to create a handle if it does not exist yet.  However, we
  // need to check that there is a corresponding grob in this
  // particular output first, and we have to do this in the global
  // context.  By far the most frequent case will be that a
  // Grob_properties for this context already exists, so we optimize
  // for that and only check the global handle when the local
  // context is pristine.
  if (check ())
    return true;
  SCM current_context_val = SCM_EOL;
  Context *g = context_->get_global_context ();
  if (!g)
    return false; // Context is probably dead

  /*
    Don't mess with MIDI.
  */
  if (g == context_
      || !g->here_defined (symbol_, &current_context_val))
    return false;

  Grob_properties *def = Grob_properties::unsmob (current_context_val);

  if (!def)
    {
      programming_error ("Grob definition expected");
      return false;
    }

  // We create the new Grob_properties from the default definition
  // since this is what we have available right now.  It may or may
  // not be accurate since we don't take into account any
  // prospective overrides in intermediate contexts.  If there are
  // any, they will be factored in when `updated' is being called.
  SCM props = Grob_properties (def->alist_, def->alist_).smobbed_copy ();
  context_->set_property (symbol_, props);
  props_ = Grob_properties::unsmob (props);
  return props_;
}

/*
  Grob descriptions (ie. alists with layout properties) are
  represented as a (ALIST . BASED-ON) pair, where BASED-ON is the
  alist defined in a parent context. BASED-ON should always be a tail
  of ALIST.

  Push or pop (depending on value of VAL) a single entry from a
  translator property list by name of PROP.  GROB_PROPERTY_PATH
  indicates nested alists, eg. '(beamed-stem-lengths details)
*/
void
Grob_property_info::push (SCM grob_property_path, SCM new_value)
{
  /*
    Don't mess with MIDI.
  */
  if (!create ())
    return;

  SCM symbol = scm_car (grob_property_path);
  if (scm_is_pair (scm_cdr (grob_property_path)))
    {
      new_value = nested_property_alist (ly_assoc_get (symbol, updated (),
                                                       SCM_EOL),
                                         scm_cdr (grob_property_path),
                                         new_value);
    }

  /* it's tempting to replace the head of the list if it's the same
   property. However, we have to keep this info around, in case we have to
   \revert back to it.
  */

  if (typecheck_grob (symbol, new_value))
    props_->alist_ = scm_acons (symbol, new_value, props_->alist_);
}

/*
  Revert the property given by property_path.
*/
void
Grob_property_info::pop (SCM grob_property_path)
{
  if (!check ())
    return;

  SCM current_alist = props_->alist_;
  SCM daddy = props_->based_on_;

  if (!scm_is_pair (grob_property_path)
      || !scm_is_symbol (scm_car (grob_property_path)))
    {
      programming_error ("Grob property path should be list of symbols.");
      return;
    }

  SCM symbol = scm_car (grob_property_path);
  if (scm_is_pair (scm_cdr (grob_property_path)))
    {
      // This is definitely wrong: the symbol must only be looked up
      // in the part of the alist before daddy.  We are not fixing
      // this right now since this is slated for complete replacement.
      SCM current_sub_alist = ly_assoc_get (symbol, current_alist, SCM_EOL);
      SCM new_val
        = nested_property_revert_alist (current_sub_alist,
                                        scm_cdr (grob_property_path));

      if (scm_is_pair (current_alist)
          && scm_caar (current_alist) == symbol
          && current_alist != daddy)
        current_alist = scm_cdr (current_alist);

      current_alist = scm_acons (symbol, new_val, current_alist);
      props_->alist_ = current_alist;
    }
  else
    {
      SCM new_alist = evict_from_alist (symbol, current_alist, daddy);

      if (new_alist == daddy)
        {
          props_ = 0;
          context_->unset_property (symbol_);
        }
      else
        props_->alist_ = new_alist;
    }
}
/*
  Convenience: a push/pop grob property using a single grob_property
  as argument.
*/
void
execute_pushpop_property (Context *context,
                          SCM grob,
                          SCM grob_property,
                          SCM new_value)
{
  Grob_property_info (context, grob).pushpop (scm_list_1 (grob_property), new_value);
}

/*
  PRE_INIT_OPS is in the order specified, and hence must be reversed.
*/
void
apply_property_operations (Context *tg, SCM pre_init_ops)
{
  for (SCM s = pre_init_ops; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM type = scm_car (entry);
      entry = scm_cdr (entry);

      if (type == ly_symbol2scm ("push"))
        {
          SCM context_prop = scm_car (entry);
          SCM val = scm_cadr (entry);
          SCM grob_prop_path = scm_cddr (entry);
          Grob_property_info (tg, context_prop).push (grob_prop_path, val);
        }
      else if (type == ly_symbol2scm ("pop"))
        {
          SCM context_prop = scm_car (entry);
          SCM grob_prop_path = scm_cdr (entry);
          Grob_property_info (tg, context_prop).pop (grob_prop_path);
        }
      else if (type == ly_symbol2scm ("assign"))
        tg->set_property (scm_car (entry), scm_cadr (entry));
      else if (type == ly_symbol2scm ("apply"))
	scm_apply_1 (scm_car (entry), tg->self_scm (), scm_cdr (entry));
      else if (type == ly_symbol2scm ("unset"))
        tg->unset_property (scm_car (entry));
    }
}

/*
  Return the object alist for SYM, checking if its base in enclosing
  contexts has changed. The alist is updated if necessary.
*/
SCM Grob_property_info::updated ()
{
  assert (scm_is_symbol (symbol_));

  Grob_property_info where = find ();

  if (!where)
    return SCM_EOL;

  Context *dad = where.context_->get_parent_context ();

  SCM daddy_props
    = dad ? Grob_property_info (dad, symbol_).updated () : SCM_EOL;

  SCM based_on = where.props_->based_on_;
  if (based_on == daddy_props)
    return where.props_->alist_;
  else
    {
      SCM copy = daddy_props;
      SCM *tail = &copy;
      SCM p = where.props_->alist_;
      while (p != based_on)
        {
          *tail = scm_cons (scm_car (p), daddy_props);
          tail = SCM_CDRLOC (*tail);
          p = scm_cdr (p);
        }

      where.props_->alist_ = copy;
      where.props_->based_on_ =  daddy_props;

      return copy;
    }
}
