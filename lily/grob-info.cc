/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "context.hh"
#include "grob-info.hh"
#include "grob.hh"
#include "item.hh"
#include "music.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Translator *t, Grob *g)
{
  origin_trans_ = t;
  grob_ = g;
  start_end_ = START;
}

Grob_info::Grob_info ()
{
  grob_ = 0;
  start_end_ = START;
  origin_trans_ = 0;
}

/* ES TODO: Junk this when no more engravers use try_music */
Music *
Grob_info::music_cause () const
{
  SCM cause = grob_->get_property ("cause");

  Music *ret = unsmob_music (cause);
  if (ret)
    return ret;
  else
    {
      Stream_event *ev = unsmob_stream_event (cause);
      if (!ev)
	return 0;
      return unsmob_music (ev->get_property ("music-cause"));
    }
}

Stream_event *
Grob_info::event_cause () const
{
  SCM cause = grob_->get_property ("cause");
  return unsmob_stream_event (cause);
}

vector<Context*>
Grob_info::origin_contexts (Translator *end) const
{
  Context *t = origin_trans_->context ();
  vector<Context*> r;
  do
    {
      r.push_back (t);
      t = t->get_parent_context ();
    }
  while (t && t != end->context ());

  return r;
}

Context *
Grob_info::context () const
{
  return origin_trans_->context ();
}

Spanner *
Grob_info::spanner () const
{
  return dynamic_cast<Spanner *> (grob_);
}

Item *
Grob_info::item () const
{
  return dynamic_cast<Item *> (grob_);
}

Stream_event *
Grob_info::ultimate_event_cause () const
{
  SCM cause = grob_->self_scm ();
  while (unsmob_grob (cause))
    {
      cause = unsmob_grob (cause)->get_property ("cause");
    }
  return unsmob_stream_event (cause);
}

/*
ES TODO: Junk this when no more engraver uses try_music
*/
Music *
Grob_info::ultimate_music_cause () const
{
  SCM cause = grob_->self_scm ();
  while (unsmob_grob (cause))
    {
      cause = unsmob_grob (cause)->get_property ("cause");
    }

  Music *ret = unsmob_music (cause);
  if (ret)
    return ret;
  else
    {
      Stream_event *ev = unsmob_stream_event (cause);
      return unsmob_music (ev->get_property ("music-cause"));
    }
}
