#include "horizontal-group-element.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"




Horizontal_group_element::Horizontal_group_element ()
  : Axis_group_element (X_AXIS,X_AXIS)
{
}

Interval
Horizontal_group_element::do_width() const
{
  return Graphical_axis_group::extent (X_AXIS);
}

