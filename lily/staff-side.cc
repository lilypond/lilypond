/*   
  g-staff-side.cc --  implement Staff_side_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-side.hh"
#include "staff-symbol.hh"
#include "debug.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"

Staff_side_element::Staff_side_element ()
{
  dir_ = CENTER;
  to_position_l_ = 0;
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);
  axis_ = Y_AXIS;
}

void
Staff_side_element::do_pre_processing ()
{
  if (!dir_)
    dir_ = get_default_direction ();

  if (axis_ == X_AXIS)
    position_self ();
}

Direction
Staff_side_element::get_default_direction () const
{
  return DOWN;
}


void
Staff_side_element::set_victim (Score_element *e)
{
  add_dependency (e);
  to_position_l_ = e;
  to_position_l_->set_parent (this, axis_);
}

void
Staff_side_element::add_support (Score_element*e)
{
  add_dependency (e);
  support_l_arr_.push (e);
}


void
Staff_side_element::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  Staff_symbol_referencer::do_substitute_element_pointer (o,n);
  if (o == to_position_l_)
    to_position_l_ = n;
  else
    support_l_arr_.unordered_substitute (o,n);
}

void
Staff_side_element::position_self ()
{
  if (to_position_l_ &&
      to_position_l_->get_elt_property (transparent_scm_sym) != SCM_BOOL_F)
    return;

  Axis other = Axis ((axis_ + 1) % NO_AXES);
  if (parent_l (axis_)->empty_b (axis_)
      &&parent_l (axis_)->empty_b (other)) // guh
    {
      warning (_("No support; erasing script"));
      to_position_l_->set_empty (X_AXIS,Y_AXIS);
      to_position_l_->set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      set_empty (X_AXIS, Y_AXIS);
      return ;
    }
  
  Interval dim;
  Graphical_element *common = 0;
  if (support_l_arr_.size ())
    {
      common = common_refpoint (typecast_array (support_l_arr_, (Graphical_element*)0),
				axis_);

      for (int i=0; i < support_l_arr_.size (); i++)
	{
	  Score_element * e = support_l_arr_ [i];
	  Real coord = e->relative_coordinate (common, axis_);

	  dim.unite (coord + e->extent (axis_));
	}
    }
  else
     common = parent_l (axis_);

  if (dim.empty_b ())
    {
      dim = Interval(0,0);
    }

  
  Interval sym_dim
    = to_position_l_
    ? to_position_l_->extent (axis_)
    : Interval(0,0);

  Real off =  relative_coordinate (common, axis_);

  SCM pad = remove_elt_property (padding_scm_sym);
  if (pad != SCM_BOOL_F)
    {
      off += gh_scm2double (SCM_CDR(pad)) * dir_;
    }
  Real total_off = dim[dir_] + off;

  /*
    no_staff_support_scm_sym is ugh bugfix to get staccato dots right.
   */
  if (to_position_l_ && to_position_l_->get_elt_property (no_staff_support_scm_sym) == SCM_BOOL_F)
     total_off += - sym_dim[-dir_];
  
  dim_cache_[axis_]->set_offset (total_off);
  if (fabs (total_off) > 100 CM)
    programming_error ("Huh ? Improbable staff side dim.");
}

void
Staff_side_element::do_post_processing ()
{
  if (axis_ == Y_AXIS)
    position_self ();
}


void
Staff_side_element::do_add_processing ()
{
  if (get_elt_property (no_staff_support_scm_sym) == SCM_BOOL_F
      && axis_ == Y_AXIS && staff_symbol_l ())
    {
      add_support (staff_symbol_l ());
    }
}

Interval
Staff_side_element::do_height () const
{
  Interval i;
  if (to_position_l_)
    return to_position_l_->extent (Y_AXIS);
  return i;
}

void
Staff_side_element::do_print () const
{
#ifndef NPRINT
  if (to_position_l_)
    DEBUG_OUT << "positioning " << to_position_l_->name();

  DEBUG_OUT << "axis == " << axis_name_str (axis_)
       << ", dir == " << to_str ((int)dir_ );
#endif
}


Interval
Staff_side_item::do_width () const
{
  Interval i;
  if (to_position_l_)
    return to_position_l_->extent (X_AXIS);
  return i;
}

void
Staff_side_item::do_print () const
{
  Staff_side_element::do_print ();
}

void
Staff_side_spanner::do_print () const
{
  Staff_side_element::do_print ();
}
