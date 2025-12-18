/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2023 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "context-def.hh"
#include "deprecated-property.hh"
#include "dispatcher.hh"
#include "grob-properties.hh"
#include "lily-imports.hh"
#include "output-def.hh"

LY_DEFINE (ly_context_alias_p, "ly:context-alias?", 2, 0, 0,
           (SCM context, SCM name),
           R"(
Is @var{name} the name or an alias of @var{context}?
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  return to_scm (tr->is_alias (name));
}

LY_DEFINE (ly_context_current_moment, "ly:context-current-moment", 1, 0, 0,
           (SCM context),
           R"(
Return the current moment of @var{context}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return to_scm (tr->now_mom ());
}

LY_DEFINE (ly_context_id, "ly:context-id", 1, 0, 0, (SCM context),
           R"(
Return the ID string of @var{context}, i.e., for @code{\context Voice = "one"
@dots{}} return the string @code{one}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return to_scm (tr->id_string ());
}

LY_DEFINE (ly_context_name, "ly:context-name", 1, 0, 0, (SCM context),
           R"(
Return the name of @var{context}, i.e., for @code{\context Voice = "one"
@dots{}} return the symbol @code{Voice}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return ly_symbol2scm (tr->context_name ());
}

LY_DEFINE (ly_context_grob_definition, "ly:context-grob-definition", 2, 0, 0,
           (SCM context, SCM name),
           R"(
Return the definition of @var{name} (a symbol) within @var{context} as an
alist.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  return Grob_property_info (tr, name).updated ();
}

LY_DEFINE (ly_context_pushpop_property, "ly:context-pushpop-property", 3, 1, 0,
           (SCM context, SCM grob, SCM eltprop, SCM val),
           R"(
Do @code{\temporary \override} or @code{\revert} operation in @var{context}.
The grob definition @var{grob} is extended with @var{eltprop} (if @var{val} is
specified) or reverted (if unspecified).
           )")
{
  auto *const tg = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob, 2);
  LY_ASSERT_TYPE (ly_is_symbol, eltprop, 3);

  execute_pushpop_property (tg, grob, eltprop, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_matched_pop_property, "ly:context-matched-pop-property",
           3, 0, 0, (SCM context, SCM grob, SCM cell),
           R"(
This undoes a particular @code{\override}, @code{\once \override} or
@code{\once \revert} when given the specific alist pair to undo.
           )")
{
  auto *const tg = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob, 2);
  Grob_property_info (tg, grob).matched_pop (cell);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_set_property_x, "ly:context-set-property!", 3, 0, 0,
           (SCM context, SCM name, SCM val),
           R"(
Set value of property @var{name} in @var{context} to @var{val}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  set_property (tr, name, val);

  return SCM_UNSPECIFIED;
}

static Context *
where_defined_with_deprecation_check (Context *ctx, SCM sym, SCM *val)
{
  auto *found = where_defined (ctx, sym, val);
  if (!found)
    {
      SCM desc = Deprecated_property::getter_desc (
        sym, Lily::deprecated_translation_getter_description);
      if (!scm_is_false (desc))
        {
          // desc is ('newSymbol new->old-value-function)
          sym = scm_car (desc);
          desc = scm_cdr (desc);
          found = where_defined (ctx, sym, val);
          if (found && val)
            {
              SCM new_to_old = scm_car (desc);
              *val = ly_call (new_to_old, *val);
            }
        }
    }
  return found;
}

LY_DEFINE_WITH_SETTER (ly_context_property, "ly:context-property",
                       ly_context_set_property_x, 2, 0, 1,
                       (SCM context, SCM name, SCM rest),
                       R"(
Get the value of property @var{name} visible in @var{context}.  The first
@var{rest} argument may optionally be an alternative value to return when the
property value is @code{'()}.  Following that, there may appear keyword options:

@indentedblock
@table @code
@item #:default
The value to return when the property is not set.  When this option is absent,
the same value is returned as when the property value is @code{'()}.
@item #:search-ancestors?
@code{#f} limits the search to @var{context}.  The default is @code{#t}.
@end table
@end indentedblock
                       )")
{
  auto *const t = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  SCM null_alternative_value = SCM_EOL;
  if (scm_is_pair (rest))
    {
      // optional non-keyword value to return when the property value is SCM_EOL
      if (SCM first = scm_car (rest); !scm_is_keyword (first))
        {
          null_alternative_value = first;
          rest = scm_cdr (rest);
        }
    }

  SCM default_value = null_alternative_value;
  SCM search_ancestors = SCM_BOOL_T;
  scm_c_bind_keyword_arguments ( //
    "ly:context-property", rest, static_cast<scm_t_keyword_arguments_flags> (0),
    ly_keyword2scm ("default"), &default_value,              //
    ly_keyword2scm ("search-ancestors?"), &search_ancestors, //
    SCM_UNDEFINED);

  SCM result = SCM_EOL;
  auto *found = where_defined_with_deprecation_check (t, name, &result);
  if (scm_is_false (search_ancestors))
    {
      // TODO: Instead of calling where_defined_with_deprecation_check and
      // ignoring a hit in an enclosing context, call a
      // here_defined_with_deprecation_check that doesn't waste time searching
      // enclosing contexts.  Cover the new code path in
      // input/regression/context-property-deprecated-*.ly.
      if (found != t)
        {
          found = nullptr;
          result = SCM_EOL;
        }
    }

  if (found)
    {
      if (!scm_is_null (result))
        return result;
      else
        return null_alternative_value;
    }

  return default_value;
}

LY_DEFINE (ly_context_property_where_defined,
           "ly:context-property-where-defined", 2, 1, 0,
           (SCM context, SCM name, SCM def),
           R"(
Return the context above @var{context} where property @var{name} is defined, or
@var{def} (defaulting to @code{'()}) if no such context is found.
           )")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  SCM value;
  tr = where_defined_with_deprecation_check (tr, name, &value);
  if (tr)
    return tr->self_scm ();
  if (SCM_UNBNDP (def))
    return SCM_EOL;
  return def;
}

LY_DEFINE (ly_context_unset_property, "ly:context-unset-property", 2, 0, 0,
           (SCM context, SCM name),
           R"(
Unset value of property @var{name} in @var{context}.
           )")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  tr->unset_property (name);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_parent, "ly:context-parent", 1, 0, 0, (SCM context),
           R"(
Return the parent of @var{context}, @code{#f} if none.
           )")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);

  tr = tr->get_parent ();
  if (tr)
    return tr->self_scm ();
  else
    return SCM_BOOL_F;
}

LY_DEFINE (ly_context_children, "ly:context-children", 1, 0, 0, (SCM context),
           R"(
Return a list with the children contexts of @var{context}.
           )")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);

  // Create a copy so that the context-internal list may not be tampered with
  // from Scheme.

  return scm_list_copy (tr->children_contexts ());
}

// TODO: Optionally constrain the search with a context ID.
LY_DEFINE (ly_context_find, "ly:context-find", 2, 0, 0, (SCM context, SCM name),
           R"(
Find a context with name or alias @var{name}, first considering @var{context}
and then searching its ancestors.  Return @code{#f} if not found.
           )")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  tr = find_context_above (tr, name);
  return tr ? tr->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_context_event_source, "ly:context-event-source", 1, 0, 0,
           (SCM context),
           R"(
Return @code{event-source} of @var{context}.
           )")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->event_source ()->self_scm ();
}

LY_DEFINE (ly_context_events_below, "ly:context-events-below", 1, 0, 0,
           (SCM context),
           R"(
Return a @code{stream-distributor} that distributes all events from
@var{context} and all its subcontexts.
           )")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->events_below ()->self_scm ();
}

LY_DEFINE (ly_context_output_def, "ly:context-output-def", 1, 0, 0,
           (SCM context),
           R"(
Return the output definition of @var{context}.
           )")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->get_output_def ()->self_scm ();
}
