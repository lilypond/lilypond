/*
  horizontal-group-item.cc -- implement Horizontal_group_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-col.hh"
#include "horizontal-group-item.hh"



void
Horizontal_group_item::do_print() const
{ 
  Axis_group_item::do_print(); 
}

Horizontal_group_item::Horizontal_group_item ()
{
  axes_[0] = axes_[1] = X_AXIS;
}
