
#include "horizontal-vertical-group-element.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

Horizontal_vertical_group_element::Horizontal_vertical_group_element()
{
  axis1_ = X_AXIS;
  axis2_ = Y_AXIS;    
}



IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group_element, Horizontal_group_element, Vertical_group_element);
