
#include "horizontal-vertical-group-elem.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

void
Horizontal_vertical_group_element::remove_all()
{
  axis_admin_.remove_all (X_AXIS,Y_AXIS);
}
void
Horizontal_vertical_group_element::add_element (Graphical_element *e)
{
  axis_admin_.add_element (e, this, X_AXIS, Y_AXIS);
}

void
Horizontal_vertical_group_element::remove_element (Graphical_element*e)
{
  axis_admin_.remove_element (e, X_AXIS, Y_AXIS);
}



IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group_element, Horizontal_group_element, Vertical_group_element);
