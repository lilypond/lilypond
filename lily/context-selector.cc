/*
  context-selector.cc -- implement Context selection.

  source file of the GNU LilyPond music typesetter
  
  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "context-selector.hh"
#include "context.hh"
#include "scm-hash.hh"

Scheme_hash_table *Context_selector::contexts_ = 0;

void
Context_selector::register_context (Context *context)
{
  if (!contexts_)
    contexts_ = new Scheme_hash_table ();
  int count = 0;
  if (Context *first = retrieve_context (identify_context (context, 0)))
    {
      count = robust_scm2int (first->get_property ("max"), 0);
      count++;
      SCM s = scm_int2num (count);
      first->set_property ("max", s);
      context->set_property ("count", s);
  }
  /* FIXME: must alway set count, for get_property () not to segfault.  */
  context->set_property ("count", scm_int2num (count));
  contexts_->set (identify_context (context, count), context->self_scm ());
}

SCM
Context_selector::identify_context (Context *context, int count)
{
  /* TODO: start time, parent-context-at-start */
  return ly_symbol2scm ((context->context_name ()
			 + ","
			 + context->id_string ()
			 + ","
			 + to_string (count)).to_str0 ());
}

SCM
Context_selector::identify_context (Context *context)
{
  return
    identify_context (context,
		      robust_scm2int (context->get_property ("count"), 0));
}

Context *
Context_selector::retrieve_context (SCM key)
{
  return unsmob_context (contexts_->get (key));
}
