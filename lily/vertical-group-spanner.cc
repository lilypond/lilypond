/*
  vertical-group-spanner.cc -- implement Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "vertical-group-spanner.hh"
#include "item.hh"
#include "p-col.hh"

Vertical_group_spanner::Vertical_group_spanner ()
{
  axes_[0] = axes_[1] = Y_AXIS;
}
