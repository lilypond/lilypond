/*
  Audio-element-info.cc -- implement Audio_element_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "audio-element-info.hh"
#include "translator-group.hh"

Audio_element_info::Audio_element_info (Audio_element*s_l, Music *r_l)
{
  elem_l_ = s_l;
  origin_trans_l_ =0;
  req_l_ = r_l;
}


Audio_element_info::Audio_element_info()
{
  elem_l_ = 0;
  req_l_ = 0;
  origin_trans_l_ =0;
}


Link_array<Translator>
Audio_element_info::origin_trans_l_arr (Translator* end) const
{
  Translator * t = origin_trans_l_;
  Link_array<Translator> r;
  do {
    r.push (t);
    t = t->daddy_trans_l_;
  } while (t && t != end->daddy_trans_l_);
  
  return r;
}
