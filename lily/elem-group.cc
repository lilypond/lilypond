/*
  elem-group.cc -- implement Horizontal_vertical_group_element

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "elem-group.hh"
#include "interval.hh"
#include "item.hh"
#include "debug.hh"

void
Axis_group_element::do_unlink()
{
    remove_all();
}


bool
Axis_group_element::contains_b(Score_elem const*e)const
{
    return axis_admin_.contains_b(e);
}

Link_array<Score_elem>
Axis_group_element::get_extra_dependencies()const
{
    return axis_admin_.elem_l_arr_;
}

void
Axis_group_element::do_print()const
{
    axis_admin_.print();
}

// ****************


void
Vertical_group_element::add_element(Score_elem*e)
{
    axis_admin_.add_element(e, this, Y_AXIS, Y_AXIS);
}

void
Vertical_group_element::remove_element(Score_elem*e)
{
    axis_admin_.remove_element(e, Y_AXIS, Y_AXIS);
}


Interval
Vertical_group_element::do_height() const
{
    return axis_admin_.extent(Y_AXIS);
}
void
Vertical_group_element::remove_all()
{
    axis_admin_.remove_all(Y_AXIS,Y_AXIS);
}
// ****************

void
Horizontal_group_element::remove_all()
{
    axis_admin_.remove_all(X_AXIS,X_AXIS);
}

void
Horizontal_group_element::add_element(Score_elem*e)
{
    axis_admin_.add_element(e,this, X_AXIS,X_AXIS);
}

void
Horizontal_group_element::remove_element(Score_elem*e)
{
    axis_admin_.remove_element(e,X_AXIS,X_AXIS);
}


Interval
Horizontal_group_element::do_width()const
{
    return axis_admin_.extent(X_AXIS);
}


// ****************

void
Horizontal_vertical_group_element::remove_all()
{
    axis_admin_.remove_all(X_AXIS,Y_AXIS);
}
void
Horizontal_vertical_group_element::add_element(Score_elem *e)
{
    axis_admin_.add_element(e, this, X_AXIS, Y_AXIS);
}

void
Horizontal_vertical_group_element::remove_element(Score_elem*e)
{
    axis_admin_.remove_element(e, X_AXIS, Y_AXIS);
}



IMPLEMENT_IS_TYPE_B1(Axis_group_element, Score_elem);
IMPLEMENT_IS_TYPE_B1(Horizontal_group_element, Axis_group_element);
IMPLEMENT_IS_TYPE_B1(Vertical_group_element, Axis_group_element);
IMPLEMENT_IS_TYPE_B2(Horizontal_vertical_group_element, Horizontal_group_element, Vertical_group_element);
