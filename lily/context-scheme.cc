/*
  context-scheme.cc -- Context bindings

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "context-def.hh"
#include "dispatcher.hh"

LY_DEFINE (ly_context_id, "ly:context-id",
	   1, 0, 0, (SCM context),
	   "Return the id string of @var{context}, "
	   "i.e. for @code{\\context Voice = one .. } "
	   "return the string @code{one}.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");

  return scm_makfrom0str (tr->id_string ().c_str ());
}

LY_DEFINE (ly_context_name, "ly:context-name",
	   1, 0, 0, (SCM context),
	   "Return the name of @var{context}, "
	   "i.e. for @code{\\context Voice = one .. } "
	   "return the symbol @code{Voice}.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");
  return ly_symbol2scm (tr->context_name ().c_str ());
}

LY_DEFINE (ly_context_grob_definition, "ly:context-grob-definition",
	   2, 0, 0, (SCM context, SCM name),
	   "Return the definition of @var{name} (a symbol) within @var{context} "
	   "as an alist")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  return updated_grob_properties (tr, name);
}

LY_DEFINE (ly_context_pushpop_property, "ly:context-pushpop-property",
	   3, 1, 0, (SCM context, SCM grob, SCM eltprop, SCM val),
	   "Do a single @code{\\override} or @code{\\revert} operation "
	   "in @var{context}.  The grob definition @code{grob} is extended "
	   "with @code{eltprop} (if @var{val} is specified) "
	   "or reverted (if  unspecified).")
{
  Context *tg = unsmob_context (context);
  SCM_ASSERT_TYPE (tg, context, SCM_ARG1, __FUNCTION__, "context");
  SCM_ASSERT_TYPE (scm_is_symbol (grob), grob, SCM_ARG2, __FUNCTION__, "symbol");
  SCM_ASSERT_TYPE (scm_is_symbol (eltprop), eltprop, SCM_ARG3, __FUNCTION__, "symbol");

  execute_pushpop_property (tg, grob, eltprop, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property, "ly:context-property",
	   2, 0, 0, (SCM c, SCM name),
	   "Return the value of @var{name} from context @var{c}")
{
  Context *t = unsmob_context (c);
  Context *tr = (t);
  SCM_ASSERT_TYPE (tr, c, SCM_ARG1, __FUNCTION__, "Translator group");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  return tr->internal_get_property (name);
}

LY_DEFINE (ly_context_set_property, "ly:context-set-property!",
	   3, 0, 0, (SCM context, SCM name, SCM val),
	   "Set value of property @var{name} in context @var{context} "
	   "to @var{val}.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  tr->set_property (name, val);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_property_where_defined, "ly:context-property-where-defined",
	   2, 0, 0, (SCM context, SCM name),
	   "Return the context above @var{context} "
	   "where @var{name} is defined.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  SCM val;
  tr = tr->where_defined (name, &val);
  if (tr)
    return tr->self_scm ();

  return SCM_EOL;
}

LY_DEFINE (ly_unset_context_property, "ly:context-unset-property", 2, 0, 0,
	   (SCM context, SCM name),
	   "Unset value of property @var{name} in context @var{context}.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

  tr->unset_property (name);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_context_parent, "ly:context-parent",
	   1, 0, 0, (SCM context),
	   "Return the parent of @var{context}, @code{#f} if none.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "Context");

  tr = tr->get_parent_context ();
  if (tr)
    return tr->self_scm ();
  else
    return SCM_BOOL_F;
}

/* FIXME: todo: should support translator IDs, and creation? */
LY_DEFINE (ly_context_find, "ly:context-find",
	   2, 0, 0, (SCM context, SCM name),
	   "Find a parent of @var{context} that has name or alias @var{name}. "
	   "Return @code{#f} if not found.")
{
  Context *tr = unsmob_context (context);
  SCM_ASSERT_TYPE (tr, context, SCM_ARG1, __FUNCTION__, "context");
  SCM_ASSERT_TYPE (scm_is_symbol (name), name, SCM_ARG2, __FUNCTION__, "symbol");

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
	   "Return now-moment of context CONTEXT")
{
  Context *ctx = unsmob_context (context);
  SCM_ASSERT_TYPE (ctx, context, SCM_ARG1, __FUNCTION__, "Context");
  return ctx->now_mom ().smobbed_copy ();
}

LY_DEFINE (ly_context_event_source, "ly:context-event-source",
           1, 0, 0, (SCM context),
           "Return event-source of context CONTEXT")
{
  Context *ctx = unsmob_context (context);
  SCM_ASSERT_TYPE (ctx, context, SCM_ARG1, __FUNCTION__, "Context");
  return ctx->event_source ()->self_scm ();
}

LY_DEFINE (ly_context_events_below, "ly:context-events-below",
           1, 0, 0, (SCM context),
           "Return a stream-distributor that distributes all events\n"
           " from @var{context} and all its subcontexts.")
{
  Context *ctx = unsmob_context (context);
  SCM_ASSERT_TYPE (ctx, context, SCM_ARG1, __FUNCTION__, "Context");
  return ctx->events_below ()->self_scm ();
}
