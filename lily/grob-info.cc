/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "grob-info.hh"

#include "grob.hh"
#include "music.hh"
#include "translator-group.hh"
#include "context.hh"

Grob_info::Grob_info ()
{
  grob_ = 0;
  origin_trans_ = 0;
}

Music *
Grob_info::music_cause ()
{
  SCM cause = grob_->get_property ("cause");
  return unsmob_music (cause);
}

Link_array<Context>
Grob_info::origin_contexts (Translator *end) const
{
  Context *t = origin_trans_->context ();
  Link_array<Context> r;
  do
    {
      r.push (t);
      t = t->get_parent_context ();
    }
  while (t && t != end->context ());

  return r;
}

