/*
  vertical-group-elem.cc -- implement Horizontal_vertical_group_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "vertical-group-elem.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

void
Vertical_group_element::add_element (Graphical_element*e)
{
  axis_admin_.add_element (e, this, Y_AXIS, Y_AXIS);
}

void
Vertical_group_element::remove_element (Graphical_element*e)
{
  axis_admin_.remove_element (e, Y_AXIS, Y_AXIS);
}


Interval
Vertical_group_element::do_height() const
{
  return axis_admin_.extent (Y_AXIS);
}
void
Vertical_group_element::remove_all()
{
  axis_admin_.remove_all (Y_AXIS,Y_AXIS);
}

IMPLEMENT_IS_TYPE_B1(Vertical_group_element, Axis_group_element);
