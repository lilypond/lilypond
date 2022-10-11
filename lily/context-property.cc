/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "smobs.hh"
#include "spanner.hh"
#include "unpure-pure-container.hh"
#include "warn.hh"

bool
typecheck_grob (SCM symbol, SCM value)
{
  if (Unpure_pure_container *upc = unsmob<Unpure_pure_container> (value))
    return typecheck_grob (symbol, upc->unpure_part ())
           && typecheck_grob (symbol, upc->pure_part ());
  return ly_is_procedure (value)
         || type_check_assignment (symbol, value,
                                   ly_symbol2scm ("backend-type?"));
}

class Grob_properties : public Simple_smob<Grob_properties>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;

private:
  friend class Grob_property_info;
  friend SCM ly_make_grob_properties (SCM);
  // alist_ may contain unexpanded nested overrides
  SCM alist_;
  // based_on_ is the cooked_ value from the next higher context that
  // alist_ is based on
  SCM based_on_;
  // cooked_ is a version of alist_ where nested overrides have been
  // expanded
  SCM cooked_;
  // cooked_from_ is the value of alist_ from which the expansion has
  // been done
  SCM cooked_from_;
  // nested_ is a count of nested overrides in alist_ Or rather: of
  // entries that must not appear in the cooked list and are
  // identified by having a "key" that is not a symbol.  Temporary
  // overrides and reverts also meet that description and have a
  // nominal key of #t/#f and a value of the original cons cell.
  int nested_;

  Grob_properties (SCM alist, SCM based_on)
    : alist_ (alist),
      based_on_ (based_on),
      // if the constructor was called with lists possibly containing
      // partial overrides, we would need to initialize with based_on in
      // order to trigger an initial update.  But this should never
      // happen, so we initialize straight with alist.
      cooked_ (alist),
      cooked_from_ (alist),
      nested_ (0)
  {
  }
};

const char *const Grob_properties::type_p_name_ = "ly:grob-properties?";

SCM
Grob_properties::mark_smob () const
{
  scm_gc_mark (alist_);
  scm_gc_mark (based_on_);
  scm_gc_mark (cooked_);
  return cooked_from_;
}

LY_DEFINE (ly_make_grob_properties, "ly:make-grob-properties", 1, 0, 0,
           (SCM alist),
           R"(
Package the given property list @var{alist} in a grob property container stored
in a context property with the name of a grob.
           )")
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
  if (auto *const c = where_defined (context_, symbol_, &res))
    if (c != context_)
      return Grob_property_info (c, symbol_, unsmob<Grob_properties> (res));
  props_ = unsmob<Grob_properties> (res);
  return *this;
}

bool
Grob_property_info::check ()
{
  if (props_)
    return true;
  SCM res = SCM_UNDEFINED;
  if (here_defined (context_, symbol_, &res))
    props_ = unsmob<Grob_properties> (res);
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
  Context *g = find_top_context (context_);
  if (!dynamic_cast<Global_context *> (g))
    return false; // Context is probably dead

  /*
    Don't mess with MIDI.
  */
  if ((g == context_) || !here_defined (g, symbol_, &current_context_val))
    return false;

  Grob_properties *def = unsmob<Grob_properties> (current_context_val);

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
  set_property (context_, symbol_, props);
  props_ = unsmob<Grob_properties> (props);
  return props_;
}

/*
  Grob descriptions (ie. alists with layout properties) are
  represented as a (ALIST . BASED-ON) pair, where BASED-ON is the
  alist defined in a parent context. BASED-ON should always be a tail
  of ALIST.

  Push a single entry from a
  translator property list by name of PROP.  GROB_PROPERTY_PATH
  indicates nested alists, eg. '(beamed-stem-lengths details)

  Return value can be passed to matched_pop and will only cancel the
  same override then.
*/
SCM
Grob_property_info::push (SCM grob_property_path, SCM new_value)
{
  /*
    Don't mess with MIDI.
  */
  if (!create ())
    return SCM_EOL;

  SCM symbol = scm_car (grob_property_path);
  SCM rest = scm_cdr (grob_property_path);
  if (scm_is_pair (rest))
    {
      // poor man's typechecking
      if (typecheck_grob (symbol, nested_create_alist (rest, new_value)))
        {
          SCM cell = scm_cons (grob_property_path, new_value);
          props_->alist_ = scm_cons (cell, props_->alist_);
          props_->nested_++;
          return cell;
        }
      return SCM_EOL;
    }

  /* it's tempting to replace the head of the list if it's the same
   property. However, we have to keep this info around, in case we have to
   \revert back to it.
  */

  if (typecheck_grob (symbol, new_value))
    {
      SCM cell = scm_cons (symbol, new_value);
      props_->alist_ = scm_cons (cell, props_->alist_);
      return cell;
    }
  return SCM_EOL;
}

// Used for \once \override, returns a token for matched_pop
SCM
Grob_property_info::temporary_override (SCM grob_property_path, SCM new_value)
{
  SCM cell = push (grob_property_path, new_value);
  if (!scm_is_pair (cell))
    return cell;
  if (scm_is_symbol (scm_car (cell)))
    props_->nested_++;
  cell = scm_cons (SCM_BOOL_T, cell);
  props_->alist_ = scm_cons (cell, scm_cdr (props_->alist_));
  return cell;
}

// Used for \once \revert, returns a token for matched_pop
SCM
Grob_property_info::temporary_revert (SCM grob_property_path)
{
  if (!check ())
    return SCM_EOL;

  SCM current_alist = props_->alist_;
  SCM daddy = props_->based_on_;
  SCM tail = SCM_EOL;

  if (!scm_is_pair (grob_property_path)
      || !scm_is_symbol (scm_car (grob_property_path)))
    {
      programming_error ("Grob property path should be list of symbols.");
      return SCM_EOL;
    }

  if (scm_is_pair (scm_cdr (grob_property_path)))
    {
      tail = assoc_tail (grob_property_path, current_alist, daddy);
      if (scm_is_false (tail))
        return SCM_EOL;
    }
  else
    {
      tail = assq_tail (scm_car (grob_property_path), current_alist, daddy);
      if (scm_is_false (tail))
        return SCM_EOL;
      ++props_->nested_;
    }

  SCM cell = scm_cons (SCM_BOOL_F, scm_car (tail));
  props_->alist_
    = partial_list_copy (current_alist, tail, scm_cons (cell, scm_cdr (tail)));
  return cell;
}

void
Grob_property_info::matched_pop (SCM cell)
{
  if (!scm_is_pair (cell))
    return;
  if (!check ())
    return;
  SCM current_alist = props_->alist_;
  SCM daddy = props_->based_on_;
  for (SCM p = current_alist; !scm_is_eq (p, daddy); p = scm_cdr (p))
    {
      if (scm_is_eq (scm_car (p), cell))
        {
          SCM key = scm_car (cell);
          if (scm_is_false (key))
            {
              // temporary revert, reactivate
              cell = scm_cdr (cell);
              if (scm_is_symbol (scm_car (cell)))
                props_->nested_--;
              props_->alist_ = partial_list_copy (current_alist, p,
                                                  scm_cons (cell, scm_cdr (p)));
              return;
            }
          if (!scm_is_symbol (key))
            props_->nested_--;
          props_->alist_ = partial_list_copy (current_alist, p, scm_cdr (p));
          return;
        }
    }
  return;
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

  if (scm_is_pair (scm_cdr (grob_property_path)))
    {
      SCM old_alist = current_alist;
      current_alist
        = evict_from_alist (grob_property_path, current_alist, daddy);
      if (scm_is_eq (old_alist, current_alist))
        return;
      props_->nested_--;
    }
  else
    current_alist
      = evict_from_alist (scm_car (grob_property_path), current_alist, daddy);

  if (scm_is_eq (current_alist, daddy))
    {
      assert (props_->nested_ == 0);
      props_ = 0;
      context_->unset_property (symbol_);
      return;
    }
  props_->alist_ = current_alist;
}
/*
  Convenience: a push/pop grob property using a single grob_property
  as argument.
*/
void
execute_pushpop_property (Context *context, SCM grob, SCM grob_property,
                          SCM new_value)
{
  Grob_property_info (context, grob)
    .pushpop (ly_list (grob_property), new_value);
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

      if (scm_is_eq (type, ly_symbol2scm ("push")))
        {
          SCM context_prop = scm_car (entry);
          SCM val = scm_cadr (entry);
          SCM grob_prop_path = scm_cddr (entry);
          Grob_property_info (tg, context_prop).push (grob_prop_path, val);
        }
      else if (scm_is_eq (type, ly_symbol2scm ("pop")))
        {
          SCM context_prop = scm_car (entry);
          SCM grob_prop_path = scm_cdr (entry);
          Grob_property_info (tg, context_prop).pop (grob_prop_path);
        }
      else if (scm_is_eq (type, ly_symbol2scm ("assign")))
        set_property (tg, scm_car (entry), scm_cadr (entry));
      else if (scm_is_eq (type, ly_symbol2scm ("apply")))
        scm_apply_1 (scm_car (entry), tg->self_scm (), scm_cdr (entry));
      else if (scm_is_eq (type, ly_symbol2scm ("unset")))
        tg->unset_property (scm_car (entry));
    }
}

/*
  Return the object alist for SYM, checking if its base in enclosing
  contexts has changed. The alist is updated if necessary.
*/
SCM
Grob_property_info::updated ()
{
  assert (scm_is_symbol (symbol_));

  Grob_property_info where = find ();

  if (!where)
    return SCM_EOL;

  Context *dad = where.context_->get_parent ();

  SCM daddy_props
    = dad ? Grob_property_info (dad, symbol_).updated () : SCM_EOL;

  SCM based_on = where.props_->based_on_;
  SCM alist = where.props_->alist_;
  if (!scm_is_eq (based_on, daddy_props))
    {
      where.props_->based_on_ = daddy_props;
      alist = partial_list_copy (alist, based_on, daddy_props);
      where.props_->alist_ = alist;
    }
  if (scm_is_eq (where.props_->cooked_from_, alist))
    return where.props_->cooked_;
  where.props_->cooked_from_ = alist;
  where.props_->cooked_ = nalist_to_alist (alist, where.props_->nested_);
  return where.props_->cooked_;
}
