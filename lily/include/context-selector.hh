/*
  context-selector.hh -- declare Context_selector

  source file of the LilyPond music typesetter
  
  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef CONTEXT_SELECTOR_HH
#define CONTEXT_SELECTOR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
 * Context_selector:
 * @register_context: register new #CONTEXT.
 #
 **/
class Context_selector
{
  static Scheme_hash_table *contexts_;

public:
  static void register_context (Context *context);
  static SCM identify_context (Context *context, int count);
  static SCM identify_context (Context *context);
  static Context *retrieve_context (SCM key);
};

#endif /* CONTEXT_SELECTOR_HH */

