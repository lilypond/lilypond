/*
  staff-info.cc -- implement Staff_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"

#include "staff-info.hh"
#include "paper-column.hh"

Staff_info::Staff_info()
{
  command_l_ =0;
  musical_l_ =0;
}


Paper_column*
Staff_info::command_pcol_l()
{
  return command_l_;
}

Paper_column*
Staff_info::musical_pcol_l()
{
  return musical_l_;
}

