/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "grob.hh"
#include "grob-info.hh"
#include "music.hh"
#include "translator-group.hh"
#include "context.hh"

Grob_info::Grob_info ()
{
  grob_ = 0;
  origin_trans_ = 0;
}

Music*
Grob_info::music_cause ()
{
  SCM cause = grob_->get_property ("cause"); 
  return unsmob_music (cause);
}

Link_array<Context>
Grob_info::origin_contexts (Translator* end) const
{
  Context * t = origin_trans_->daddy_context_;
  Link_array<Context> r;
  do {
    r.push (t);
    t = t->daddy_context_;
  } while (t && t != end->daddy_context_);
  
  return r;
}
  
