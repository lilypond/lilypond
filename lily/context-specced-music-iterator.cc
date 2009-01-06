/*
  context-specced-music-iterator.cc -- implement
  Context_specced_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "music-wrapper-iterator.hh"
#include "context.hh"
#include "music.hh"


class Context_specced_music_iterator : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  virtual void construct_children ();
};

void
Context_specced_music_iterator::construct_children ()
{
  SCM ct = get_music ()->get_property ("context-type");

  string c_id;
  SCM ci = get_music ()->get_property ("context-id");
  if (scm_is_string (ci))
    c_id = ly_scm2string (ci);
  SCM ops = get_music ()->get_property ("property-operations");

  Context *a = 0;

  if (to_boolean (get_music ()->get_property ("create-new")))
    a = get_outlet ()->create_unique_context (ct, c_id, ops);
  else
    a = get_outlet ()->find_create_context (ct, c_id, ops);

  if (a
      && to_boolean (get_music ()->get_property ("descend-only"))
      && !is_child_context (get_outlet (), a))
    a = 0;

  if (a)
    set_context (a);

  Music_wrapper_iterator::construct_children ();
}

IMPLEMENT_CTOR_CALLBACK (Context_specced_music_iterator);
