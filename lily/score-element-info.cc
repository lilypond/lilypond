/*
  score-element-info.cc -- implement Score_element_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score-element-info.hh"
#include "request.hh"

Score_element_info::Score_element_info (Score_element*s_l, Request*r_l)
{
  elem_l_ = s_l;
  req_l_ = r_l;
}


Score_element_info::Score_element_info()
{
  elem_l_ = 0;
  req_l_ = 0;
}



