/*
  score-elem-info.cc -- implement Score_elem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "score-elem-info.hh"
#include "request.hh"

Score_elem_info::Score_elem_info (Score_elem*s_l, Request*r_l)
{
  elem_l_ = s_l;
  req_l_ = r_l;
}

Score_elem_info::Score_elem_info()
{
  elem_l_ = 0;
  req_l_ = 0;
}



