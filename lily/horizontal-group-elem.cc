
#include "horizontal-group-elem.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B1(Horizontal_group_element, Axis_group_element);

void
Horizontal_group_element::remove_all()
{
  axis_admin_.remove_all (X_AXIS,X_AXIS);
}

void
Horizontal_group_element::add_element (Graphical_element*e)
{
  axis_admin_.add_element (e,this, X_AXIS,X_AXIS);
}

void
Horizontal_group_element::remove_element (Graphical_element*e)
{
  axis_admin_.remove_element (e,X_AXIS,X_AXIS);
}


Interval
Horizontal_group_element::do_width() const
{
  return axis_admin_.extent (X_AXIS);
}

