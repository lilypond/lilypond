/*
  Audio-element-info.cc -- implement Audio_element_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "audio-element-info.hh"
#include "translator-group.hh"

Audio_element_info::Audio_element_info (Audio_element*s, Music *r)
{
  elem_ = s;
  origin_trans_ =0;
  req_ = r;
}


Audio_element_info::Audio_element_info ()
{
  elem_ = 0;
  req_ = 0;
  origin_trans_ =0;
}


Link_array<Translator>
Audio_element_info::origin_transes (Translator* end) const
{
  Translator * t = origin_trans_;
  Link_array<Translator> r;
  do {
    r.push (t);
    t = t->daddy_trans_;
  } while (t && t != end->daddy_trans_);
  
  return r;
}
