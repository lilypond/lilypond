/*
  grob-info.cc -- implement Grob_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "grob-info.hh"
#include "request.hh"
#include "translator.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Grob*s_l, SCM c)
{
  grob_l_ = s_l;
  cause_ = c; 
  origin_trans_l_ = 0;  
}


Grob_info::Grob_info ()
{
  grob_l_ = 0;
  cause_ = SCM_EOL;
  origin_trans_l_ = 0;
}

Music*
Grob_info::music_cause ()
{
  return unsmob_music (cause_);
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
  
