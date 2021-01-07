/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>
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
#include "dispatcher.hh"
#include "grob-properties.hh"

LY_DEFINE (ly_context_current_moment,
           "ly:context-current-moment",
           1, 0, 0, (SCM context),
           "Return the current moment of @var{context}.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return tr->now_mom ().smobbed_copy ();
}

LY_DEFINE (ly_context_id, "ly:context-id",
           1, 0, 0, (SCM context),
           "Return the ID string of @var{context},"
           " i.e., for @code{\\context Voice = \"one\" @dots{}}"
           " return the string @code{one}.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return ly_string2scm (tr->id_string ());
}

LY_DEFINE (ly_context_name, "ly:context-name",
           1, 0, 0, (SCM context),
           "Return the name of @var{context},"
           " i.e., for @code{\\context Voice = \"one\" @dots{}}"
           " return the symbol @code{Voice}.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);

  return ly_symbol2scm (tr->context_name ().c_str ());
}

LY_DEFINE (ly_context_grob_definition, "ly:context-grob-definition",
           2, 0, 0, (SCM context, SCM name),
           "Return the definition of @var{name} (a symbol) within"
           " @var{context} as an alist.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  return Grob_property_info (tr, name).updated ();
}

LY_DEFINE (ly_context_pushpop_property, "ly:context-pushpop-property",
           3, 1, 0, (SCM context, SCM grob, SCM eltprop, SCM val),
           "Do @code{\\temporary \\override} or @code{\\revert} operation"
           " in @var{context}.  The grob definition @var{grob} is extended"
           " with @var{eltprop} (if @var{val} is specified) or reverted"
           " (if unspecified).")
{
  auto *const tg = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob, 2);
  LY_ASSERT_TYPE (ly_is_symbol, eltprop, 3);

  execute_pushpop_property (tg, grob, eltprop, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_matched_pop_property, "ly:context-matched-pop-property",
           3, 0, 0, (SCM context, SCM grob, SCM cell),
           "This undoes a particular @code{\\override},"
           " @code{\\once \\override} or @code{\\once \\revert}"
           " when given the specific alist pair to undo.")
{
  auto *const tg = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob, 2);
  Grob_property_info (tg, grob).matched_pop (cell);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property, "ly:context-property",
           2, 1, 0, (SCM context, SCM sym, SCM def),
           "Return the value for property @var{sym} in @var{context}."
           " If @var{def} is given, and property value is @code{'()},"
           " return @var{def}.")
{
  auto *const t = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  SCM result = get_property (t, sym);
  return !SCM_UNBNDP (def) && scm_is_null (result) ? def : result;
}

LY_DEFINE (ly_context_set_property_x, "ly:context-set-property!",
           3, 0, 0, (SCM context, SCM name, SCM val),
           "Set value of property @var{name} in context @var{context}"
           " to @var{val}.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  set_property (tr, name, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property_where_defined, "ly:context-property-where-defined",
           2, 0, 0, (SCM context, SCM name),
           "Return the context above @var{context}"
           " where @var{name} is defined.")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  SCM val;
  tr = tr->where_defined (name, &val);
  if (tr)
    return tr->self_scm ();

  return SCM_EOL;
}

LY_DEFINE (ly_context_unset_property, "ly:context-unset-property", 2, 0, 0,
           (SCM context, SCM name),
           "Unset value of property @var{name} in context @var{context}.")
{
  auto *const tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  tr->unset_property (name);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_parent, "ly:context-parent",
           1, 0, 0, (SCM context),
           "Return the parent of @var{context}, @code{#f} if none.")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);

  tr = tr->get_parent ();
  if (tr)
    return tr->self_scm ();
  else
    return SCM_BOOL_F;
}

/* FIXME: todo: should support translator IDs, and creation? */
LY_DEFINE (ly_context_find, "ly:context-find",
           2, 0, 0, (SCM context, SCM name),
           "Find a parent of @var{context} that has name or alias @var{name}."
           "  Return @code{#f} if not found.")
{
  auto *tr = LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  tr = find_context_above (tr, name);
  return tr ? tr->self_scm () : SCM_BOOL_F;
}

LY_DEFINE (ly_context_now, "ly:context-now",
           1, 0, 0, (SCM context),
           "Return @code{now-moment} of context @var{context}.")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->now_mom ().smobbed_copy ();
}

LY_DEFINE (ly_context_event_source, "ly:context-event-source",
           1, 0, 0, (SCM context),
           "Return @code{event-source} of context @var{context}.")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->event_source ()->self_scm ();
}

LY_DEFINE (ly_context_events_below, "ly:context-events-below",
           1, 0, 0, (SCM context),
           "Return a @code{stream-distributor} that distributes all events"
           " from @var{context} and all its subcontexts.")
{
  auto *const ctx = LY_ASSERT_SMOB (Context, context, 1);
  return ctx->events_below ()->self_scm ();
}
