/*   
  g-staff-side.cc --  implement G_staff_side_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "g-staff-side.hh"

G_staff_side_item::G_staff_side_item ()
{
  dir_ = CENTER;
  to_position_l_ = 0;
  transparent_b_ = true;
  padding_f_ = 0;
  axis_ = Y_AXIS;
}


void
G_staff_side_item::do_pre_processing ()
{
  if (!dir_)
    set_default_direction ();

  if (axis_ == X_AXIS)
    position_self ();
}

void
G_staff_side_item::set_default_direction ()
{
  dir_ = DOWN;
}


void
G_staff_side_item::set_victim (Score_element *e)
{
  add_dependency (e);
  to_position_l_ = e;
  to_position_l_->dim_cache_[axis_].parent_l_ = &dim_cache_[axis_];
}

void
G_staff_side_item::add_support (Score_element*e)
{
  add_dependency (e);
  support_l_arr_.push (e);
}


void
G_staff_side_item::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (o == to_position_l_)
    to_position_l_ = n;
  else
    support_l_arr_.unordered_substitute (o,n);
}

void
G_staff_side_item::position_self ()
{
  Interval dim;
  Dimension_cache *common = 0;
  if (support_l_arr_.size ())
    {
      common = common_group (typecast_array (support_l_arr_, (Graphical_element*)0),
			     axis_);

      for (int i=0; i < support_l_arr_.size (); i++)
	{
	  Score_element * e = support_l_arr_ [i];
	  Real coord = e->relative_coordinate (common, axis_);
	  dim.unite (coord + e->extent (axis_));
	}
    }
  else
    {
      dim = Interval(0,0);
      common = dim_cache_[axis_].parent_l_;
    }
  Interval sym_dim = to_position_l_->extent (axis_);
  Real off = dim_cache_[axis_].relative_coordinate (common) - padding_f_ * dir_;
  
  dim_cache_[axis_].set_offset (dim[dir_] - sym_dim[-dir_] - off);
}

void
G_staff_side_item::do_post_processing ()
{
  if (axis_ == Y_AXIS)
    position_self ();
}

