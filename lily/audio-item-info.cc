/*
  Audio-element-info.cc -- implement Audio_element_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "audio-element-info.hh"
#include "request.hh"

Audio_element_info::Audio_element_info (Audio_element*s_l, Music *r_l)
{
  elem_l_ = s_l;
  req_l_ = r_l;
}


Audio_element_info::Audio_element_info()
{
  elem_l_ = 0;
  req_l_ = 0;
}



