/*
  vertical-group-elem.cc -- implement Horizontal_vertical_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "vertical-group-element.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"


Vertical_group_element::Vertical_group_element()
  : Axis_group_element (Y_AXIS,Y_AXIS)
{
}

Interval
Vertical_group_element::do_height() const
{
  return Graphical_axis_group::extent (Y_AXIS);
}


