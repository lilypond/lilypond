/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "grob.hh"
#include "grob-info.hh"
#include "music.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Grob*s)
{
  grob_ = s;
  origin_trans_ = 0;  
}


Grob_info::Grob_info ()
{
  grob_ = 0;
  origin_trans_ = 0;
}

Music*
Grob_info::music_cause ()
  
{
  SCM cause = grob_->get_grob_property ("cause"); 
  return unsmob_music (cause);
}

Link_array<Translator>
Grob_info::origin_transes (Translator* end) const
{
  Translator * t = origin_trans_;
  Link_array<Translator> r;
  do {
    r.push (t);
    t = t->daddy_trans_;
  } while (t && t != end->daddy_trans_);
  
  return r;
}
  
