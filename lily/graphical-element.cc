/*
  graphical-element.cc -- implement Graphical_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "graphical-element.hh"
#include "axis-group-element.hh"
#include "debug.hh"

bool
Graphical_element::empty_b () const
{
  return empty_b_; 
}

Graphical_element::Graphical_element ()
{
  init ();
}

Graphical_element::Graphical_element (Graphical_element const &s)
{
  init ();
  empty_b_ = s.empty_b_;
  axis_group_l_a_[0] = axis_group_l_a_[1] =0;
  offset_ = Offset (0,0);
} 

Graphical_element::~Graphical_element ()
{
  
}

void
Graphical_element::init ()
{
  empty_b_ = false;
  axis_group_l_a_[X_AXIS] = axis_group_l_a_[Y_AXIS] =0;
  offset_ = Offset (0,0);
  cached_valid_b_a_ [X_AXIS] = cached_valid_b_a_[Y_AXIS] = false;
}

void
Graphical_element::invalidate_cache (Axis a)
{
  Graphical_element * g = this;
  while (g && g->cached_valid_b_a_[a])
    {
      g->cached_valid_b_a_ [a] = false;  
      g = g->axis_group_l_a_[a];
    }
}

Real
Graphical_element::absolute_coordinate (Axis a) const
{
  Real r = offset_[a];
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
       axis_group_l; axis_group_l = axis_group_l->axis_group_l_a_[a])
	
    r += axis_group_l->offset_[a];
  return r;
}
 

Offset
Graphical_element::absolute_offset() const
{
  return Offset (absolute_coordinate (X_AXIS), absolute_coordinate (Y_AXIS));
}

void
Graphical_element::translate_axis (Real y, Axis a)
{
  if (axis_group_l_a_[a])
    axis_group_l_a_[a]->invalidate_cache (a);
  offset_[a] += y;
}

Real
Graphical_element::relative_coordinate (Axis_group_element*e, Axis a) const
{
  Real r =0.0;
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
       axis_group_l != e;
       axis_group_l = axis_group_l->axis_group_l_a_[a])
    r +=  axis_group_l->offset_[a];

  return r;
}

Axis_group_element* 
Graphical_element::common_group (Graphical_element const* s, Axis a) const
{
  Link_array<Axis_group_element> my_groups;
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
       axis_group_l;
       axis_group_l = axis_group_l->axis_group_l_a_[a])
    my_groups.push (axis_group_l);

  Axis_group_element* common_l=0;
  for (Axis_group_element * axis_group_l = s->axis_group_l_a_[a];
       !common_l && axis_group_l;
       axis_group_l = axis_group_l->axis_group_l_a_[a])
    common_l = my_groups.find_l (axis_group_l);

  return common_l;
}



void
Graphical_element::translate (Offset offset)
{
  translate_axis (offset[Y_AXIS], Y_AXIS);
  translate_axis (offset[X_AXIS], X_AXIS);
}

Interval
Graphical_element::width() const
{
  return extent (X_AXIS);
}

void
Graphical_element::set_empty (bool b)
{
  if (empty_b_ != b)
    {
      empty_b_ = b;
      if (!empty_b_)
	{
	  invalidate_cache (X_AXIS);
	  invalidate_cache (Y_AXIS);
	}
    }
  
}

Interval
Graphical_element::extent (Axis a) const
{
  if (empty_b_)
    return Interval ();
  
  if (!cached_valid_b_a_[a])
    {
      Graphical_element *self = (Graphical_element*)this;
      self->cached_dimension_a_[a] = (a == X_AXIS)? do_width(): do_height ();
      self->cached_valid_b_a_[a] = true;
    }
  
  Interval r(cached_dimension_a_[a]);
  if (!r.empty_b()) // float exception on DEC Alpha
    r+=offset_[a];

  return r;
}

Interval
Graphical_element::height() const
{
  return extent (Y_AXIS);
}

void
Graphical_element::unlink ()
{
    for (int j=0; j < 2; j++)
    if (axis_group_l_a_[j])
      axis_group_l_a_[j]->remove_element (this);
}

void
Graphical_element::junk_links ()
{
    axis_group_l_a_[X_AXIS] = axis_group_l_a_[Y_AXIS] =0;
}

void
Graphical_element::print () const
{
#ifndef NPRINT
  if (offset_.x() || offset_.y ())
    DOUT << "offset: " << offset_.str() ;
  DOUT << "\n";
#endif
}

IMPLEMENT_IS_TYPE_B(Graphical_element);

