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
      count = robust_scm2int (first->get_property ("tweakCount"), 0);
      count++;
      SCM s = scm_int2num (count);
      first->set_property ("tweakCount", s);
      context->set_property ("tweakRank", s);
  }
  /* FIXME: must alway set rank, for get_property () not to segfault.  */
  context->set_property ("tweakRank", scm_int2num (count));
  store_context (identify_context (context, count), context);
}

SCM
Context_selector::identify_context (Context *context, int count)
{
  /* TODO: start time, parent-context-at-start */
  return scm_list_3 (scm_makfrom0str (context->context_name ().to_str0 ()),
		     scm_makfrom0str (context->id_string ().to_str0 ()),
		     scm_int2num (count));
}

SCM
Context_selector::identify_context (Context *context)
{
  return
    identify_context (context,
		      robust_scm2int (context->get_property ("tweakRank"), 0));
}

void
Context_selector::store_context (SCM context_id, Context *context)
{
  contexts_->set (ly_to_symbol (context_id), context->self_scm ());
}

Context *
Context_selector::retrieve_context (SCM context_id)
{
  return unsmob_context (contexts_->get (ly_to_symbol (context_id)));
}
