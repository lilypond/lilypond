/*
  vertical-align-spanner.cc -- implement Vertical_align_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "vertical-align-spanner.hh"




Vertical_align_spanner::Vertical_align_spanner ()
{
  axis_ = Y_AXIS;
}

void
Vertical_align_spanner::do_print ()const
{
    Align_element::do_print () ;
}
