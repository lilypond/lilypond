/*
  vertical-group-spanner.cc -- implement Vertical_group_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "vertical-group-spanner.hh"
#include "item.hh"
#include "p-col.hh"

IMPLEMENT_IS_TYPE_B2(Vertical_group_spanner, Axis_group_spanner, Vertical_group_element);


Vertical_group_spanner::Vertical_group_spanner ()
{
  axis1_ = axis2_ = Y_AXIS;
}
