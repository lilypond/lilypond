/*
  line-spacer.cc -- implement Line_spacer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "line-spacer.hh"
#include "dimensions.hh"

Line_spacer::Line_spacer()
{
  indent_f_ =0.0;
  default_space_f_ = 20 PT;
}


Line_spacer::~Line_spacer ()
{
}

