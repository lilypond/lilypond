/*
  context-scheme.cc -- Context bindings

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "context-def.hh"
#include "dispatcher.hh"

LY_DEFINE (ly_context_id, "ly:context-id",
	   1, 0, 0, (SCM context),
	   "Return the ID string of @var{context},"
	   " i.e., for @code{\\context Voice = one @dots{}}"
	   " return the string @code{one}.")
{
  Context *tr = unsmob_context (context);

  LY_ASSERT_SMOB (Context, context, 1);

  return ly_string2scm (tr->id_string ());
}

LY_DEFINE (ly_context_name, "ly:context-name",
	   1, 0, 0, (SCM context),
	   "Return the name of @var{context},"
	   " i.e., for @code{\\context Voice = one @dots{}}"
	   " return the symbol @code{Voice}.")
{
  LY_ASSERT_SMOB (Context, context, 1);

  Context *tr = unsmob_context (context);

  return ly_symbol2scm (tr->context_name ().c_str ());
}

LY_DEFINE (ly_context_grob_definition, "ly:context-grob-definition",
	   2, 0, 0, (SCM context, SCM name),
	   "Return the definition of @var{name} (a symbol) within"
	   " @var{context} as an alist.")
{
  Context *tr = unsmob_context (context);
  
  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  return updated_grob_properties (tr, name);
}

LY_DEFINE (ly_context_pushpop_property, "ly:context-pushpop-property",
	   3, 1, 0, (SCM context, SCM grob, SCM eltprop, SCM val),
	   "Do a single @code{\\override} or @code{\\revert} operation"
	   " in @var{context}.  The grob definition @var{grob} is extended"
	   " with @var{eltprop} (if @var{val} is specified) or reverted"
	   " (if unspecified).")
{
  Context *tg = unsmob_context (context);

  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, grob, 2);
  LY_ASSERT_TYPE (ly_is_symbol, eltprop, 3);

  execute_pushpop_property (tg, grob, eltprop, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property, "ly:context-property",
	   2, 0, 0, (SCM c, SCM name),
	   "Return the value of @var{name} from context @var{c}.")
{
  LY_ASSERT_SMOB (Context, c, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  Context *t = unsmob_context (c);
  return t->internal_get_property (name);
}

LY_DEFINE (ly_context_set_property_x, "ly:context-set-property!",
	   3, 0, 0, (SCM context, SCM name, SCM val),
	   "Set value of property @var{name} in context @var{context}"
	   " to @var{val}.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);

  Context *tr = unsmob_context (context);

  tr->set_property (name, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property_where_defined, "ly:context-property-where-defined",
	   2, 0, 0, (SCM context, SCM name),
	   "Return the context above @var{context}"
	   " where @var{name} is defined.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);
  
  Context *tr = unsmob_context (context);

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
  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);
  Context *tr = unsmob_context (context);
  
  tr->unset_property (name);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_parent, "ly:context-parent",
	   1, 0, 0, (SCM context),
	   "Return the parent of @var{context}, @code{#f} if none.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  Context *tr = unsmob_context (context);

  tr = tr->get_parent_context ();
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
  LY_ASSERT_SMOB (Context, context, 1);
  LY_ASSERT_TYPE (ly_is_symbol, name, 2);
  Context *tr = unsmob_context (context);

  while (tr)
    {
      if (tr->is_alias (name))
	return tr->self_scm ();
      tr = tr->get_parent_context ();
    }

  return SCM_BOOL_F;
}

LY_DEFINE (ly_context_now, "ly:context-now",
	   1, 0, 0, (SCM context),
	   "Return @code{now-moment} of context @var{context}.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  Context *ctx = unsmob_context (context);
  return ctx->now_mom ().smobbed_copy ();
}

LY_DEFINE (ly_context_event_source, "ly:context-event-source",
           1, 0, 0, (SCM context),
           "Return @code{event-source} of context @var{context}.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  Context *ctx = unsmob_context (context);
  return ctx->event_source ()->self_scm ();
}

LY_DEFINE (ly_context_events_below, "ly:context-events-below",
           1, 0, 0, (SCM context),
           "Return a @code{stream-distributor} that distributes all events"
           " from @var{context} and all its subcontexts.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  Context *ctx = unsmob_context (context);
  return ctx->events_below ()->self_scm ();
}
