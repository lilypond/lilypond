/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "grob.hh"
#include "grob-info.hh"
#include "request.hh"
#include "translator.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Grob*s_l)
{
  grob_l_ = s_l;
  origin_trans_l_ = 0;  
}


Grob_info::Grob_info ()
{
  grob_l_ = 0;
  origin_trans_l_ = 0;
}

Music*
Grob_info::music_cause ()
  
{
  SCM cause = grob_l_->get_grob_property ("cause"); 
  return unsmob_music (cause);
}

Link_array<Translator>
Grob_info::origin_trans_l_arr (Translator* end) const
{
  Translator * t = origin_trans_l_;
  Link_array<Translator> r;
  do {
    r.push (t);
    t = t->daddy_trans_l_;
  } while (t && t != end->daddy_trans_l_);
  
  return r;
}
  
