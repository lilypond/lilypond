/*
  staff-side.cc -- implement Staff_side

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "interval.hh"
#include "paper-def.hh"
#include "staff-side.hh"
#include "staff-sym.hh"
#include "debug.hh"
#include "dimensions.hh"


Staff_side::Staff_side()
{
  coordinate_offset_f_=0;
  sym_int_ = Interval (0,0);
  padding_f_ = 0.0;
  dir_ = CENTER;
  axis_ = Y_AXIS;
}


Interval
Staff_side::support_extent() const
{
  Interval y_int;
  for (int i=0; i < support_l_arr_.size(); i++) 
    {
    Graphical_axis_group *common = 
	common_group (support_l_arr_[i], axis_);
	
      Real y = support_l_arr_[i]->relative_coordinate (common, axis_)  
	-relative_coordinate (common,axis_);

      y_int.unite (y + support_l_arr_[i]->extent(axis_));
    }

  if (y_int.empty_b())
    {
      y_int = Interval (0,0);
    }
  return Interval(y_int[LEFT] - padding_f_, y_int[RIGHT] + padding_f_);
}

void
Staff_side::add_support (Score_element*i)
{
  support_l_arr_.push (i);
  add_dependency (i);
}

Real
Staff_side::get_position_f () const
{
  if (!dir_)
    {
      warning (_ ("Staff_side::get_position_f(): "
		 "somebody forgot to set my direction, returning -20"));
      return -20;
    }


  Real y = 0;
  Real inter_f = paper()-> internote_f ();

  Interval v = support_extent();

  // ugh, dim[y] = PT over here
  y = v[dir_] + 1 * dir_ * inter_f;

  int coordinate_offset_f_i = (int)rint (y / inter_f);
  // ugh: 5 -> staff_lines
  if (axis_ == Y_AXIS && abs (coordinate_offset_f_i) < 5)
    {
      if (!(abs (coordinate_offset_f_i) % 2))
	y += (Real)dir_ * inter_f;
    }
//  else
//    y = v[dir_] + 1 * dir_ * inter_f;

  return y;
}

Interval
Staff_side::symbol_height() const
{
  return Interval (0,0);
}

Interval
Staff_side::symbol_width () const
{
  return Interval (0,0);
}

void
Staff_side::do_pre_processing ()
{
  if (axis_== X_AXIS)
    do_side_processing ();
}

void
Staff_side::do_side_processing ()
{
  sym_int_ = symbol_extent();
  coordinate_offset_f_ = get_position_f();
  if (dir_)
    coordinate_offset_f_ += - sym_int_[-dir_];

}

/*
  ugh should use do_width (), do_height (), get_extent ()
 */
Interval
Staff_side::symbol_extent () const
{
  if (axis_ == Y_AXIS)
    return symbol_height ();
  else
    return symbol_width ();
}


void
Staff_side::do_post_processing()
{
  if (axis_ == Y_AXIS)
    do_side_processing ();
}

void
Staff_side::do_substitute_dependency (Score_element*o, Score_element*n)
{
  support_l_arr_.unordered_substitute (o,n);
}


IMPLEMENT_IS_TYPE_B1(Staff_side, Score_element);
