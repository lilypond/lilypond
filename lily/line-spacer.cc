/*
  line-spacer.cc -- implement Line_spacer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "line-spacer.hh"

Line_spacer::Line_spacer()
{
  paper_l_ =0;
}
Paper_def*
Line_spacer::paper_l() const
{ 
  return paper_l_ ;
}

Line_spacer::~Line_spacer ()
{
}
