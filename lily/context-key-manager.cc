/*
  context-key-manager.cc -- implement Context_key_manager 

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "context-key-manager.hh"
#include "object-key.hh"
#include "lilypond-key.hh"
#include "main.hh"

Context_key_manager::Context_key_manager (Object_key const *key)
{
  key_ = key;
}

void
Context_key_manager::unprotect () const
{
  if (key_)
    ((Object_key *)key_)->unprotect ();
}


Object_key const *
Context_key_manager::get_context_key (Moment now, string type, string id)
{
  if (!use_object_keys)
    return 0;

  string now_key = type + "@" + id;

  int disambiguation_count = 0;
  if (context_counts_.find (now_key) != context_counts_.end ())
    disambiguation_count = context_counts_[now_key];

  context_counts_[now_key] = disambiguation_count + 1;

  return new Lilypond_context_key (key (),
				   now,
				   type, id,
				   disambiguation_count);
}


Object_key const *
Context_key_manager::get_grob_key (Moment m, string name)
{
  if (!use_object_keys)
    return 0;

  return create_grob_key (m, name);
}

/*
  We want to have a key for some objects anyway, so we can invent a
  unique identifier for each (book,score) tuple.
*/
Object_key const *
Context_key_manager::create_grob_key (Moment now, string name)
{
  int disambiguation_count = 0;
  if (grob_counts_.find (name) != grob_counts_.end ())
    disambiguation_count = grob_counts_[name];
  grob_counts_[name] = disambiguation_count + 1;

  Object_key *k = new Lilypond_grob_key (key (),
					 now,
					 name,
					 disambiguation_count);

  return k;
}

void
Context_key_manager::gc_mark () const
{
  if (key_)
    scm_gc_mark (key_->self_scm ());

}

void
Context_key_manager::clear ()
{
  if (!use_object_keys)
    return;

  grob_counts_.clear ();
  context_counts_.clear ();
}

Context_key_manager::Context_key_manager (Context_key_manager const &src)
{
  (void)src;
  assert (false);
}
