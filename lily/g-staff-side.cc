/*   
  g-staff-side.cc --  implement G_staff_side_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "g-staff-side.hh"

void
G_staff_side_item::do_pre_processing ()
{
  if (!dir_)
    set_default_direction ();
}

void
G_staff_side_item::set_default_direction ()
{
  dir_ = DOWN;
}

G_staff_side_item::G_staff_side_item ()
{
  dir_ = CENTER;
  to_position_l_ = 0;
  transparent_b_ = true;
}

void
G_staff_side_item::set_victim (Score_element *e)
{
  add_dependency (e);
  to_position_l_ = e;
  to_position_l_->dim_cache_[Y_AXIS].parent_l_ = &dim_cache_[Y_AXIS];
}

void
G_staff_side_item::add_support (Score_element*e)
{
  add_dependency (e);
  support_l_arr_.push (e);
}


void
G_staff_side_item::do_substitute_dependency (Score_element*o, Score_element*n)
{
  if (o == to_position_l_)
    to_position_l_ = n;
  else
    support_l_arr_.unordered_substitute (o,n);
}


void
G_staff_side_item::do_post_processing ()
{
  Dimension_cache *common = common_group (typecast_array (support_l_arr_, (Graphical_element*)0),
					  Y_AXIS);

  Interval dim;
  for (int i=0; i < support_l_arr_.size (); i++)
    {
      Score_element * e = support_l_arr_ [i];
      Real coord = e->relative_coordinate (common, Y_AXIS);
      dim.unite (coord + e->extent (Y_AXIS));
    }
  if (!support_l_arr_.size ())
    dim = Interval (0,0);

  Interval sym_dim = to_position_l_->extent (Y_AXIS);
  Real off = dim_cache_[Y_AXIS].relative_coordinate (common);
  
  dim_cache_[Y_AXIS].set_offset (dim[dir_] - sym_dim[-dir_] - off);
}
