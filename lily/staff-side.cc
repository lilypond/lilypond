/*
  staff-side.cc -- implement Staff_side

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "interval.hh"
#include "paper-def.hh"
#include "dimen.hh"
#include "staff-side.hh"
#include "staff-sym.hh"
#include "debug.hh"


Staff_side::Staff_side()
{
  y_=0;
  sym_int_ = Interval (0,0);
  dir_ = CENTER;
}


Interval
Staff_side::support_height() const
{
  Interval y_int;
  for (int i=0; i < support_l_arr_.size(); i++) 
    {
      Axis_group_element *common = 
	common_group (support_l_arr_[i], Y_AXIS);
	
      Real y = support_l_arr_[i]->relative_coordinate (common, Y_AXIS)  
	-relative_coordinate (common,Y_AXIS);

      y_int.unite (y + support_l_arr_[i]->height());
    }


  if (y_int.empty_b())
    {
      y_int = Interval (0,0);
    }
  return y_int;
}

void
Staff_side::add_support (Score_elem*i)
{
  support_l_arr_.push (i);
  add_dependency (i);
}

Real
Staff_side::get_position_f () const
{
  if (!dir_)
    {
      warning (_("Staff_side::get_position_f(): "
		 "somebody forgot to set my vertical direction, returning -20"));
      return -20;
    }


  Real y=0;
  Real inter_f = paper()-> internote_f ();

  Interval v= support_height();
  y = v[dir_] + 1 * dir_ * inter_f;	// ugh

  return y;
}

Interval
Staff_side::symbol_height() const
{
  return Interval (0,0);
}

void
Staff_side::do_post_processing()
{
  sym_int_ = symbol_height();
  y_ = get_position_f();
  if (dir_)
    y_ += - sym_int_[-dir_];
}

void
Staff_side::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  support_l_arr_.unordered_substitute (o,n);
}


IMPLEMENT_IS_TYPE_B1(Staff_side, Score_elem);
