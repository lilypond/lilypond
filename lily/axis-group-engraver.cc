/*   
  axis-group-engraver.cc --  implement Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "axis-group-engraver.hh"
#include "axis-group-spanner.hh"
#include "paper-column.hh"

Axis_group_engraver::Axis_group_engraver ()
{
  staffline_p_ = 0;
}

void
Axis_group_engraver::do_creation_processing ()
{
  staffline_p_ = get_spanner_p ();
  staffline_p_->set_axes (Y_AXIS, Y_AXIS);
  staffline_p_->set_bound(LEFT,get_staff_info().command_pcol_l ());
  announce_element (Score_element_info (staffline_p_, 0));
}

Axis_group_spanner*
Axis_group_engraver::get_spanner_p () const
{
  return new Axis_group_spanner;
}
void
Axis_group_engraver::do_removal_processing ()
{
  staffline_p_->set_bound(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (staffline_p_);
  staffline_p_ = 0;
}

void
Axis_group_engraver::acknowledge_element (Score_element_info i)
{
  elts_.push (i.elem_l_);
}

void
Axis_group_engraver::process_acknowledged ()
{
  /* UGH UGH UGH */
  for (int i=0; i < elts_.size (); i++)
    {
      if (!elts_[i]->parent_l (Y_AXIS))
	staffline_p_->add_element (elts_[i]);
      else
	if (
	    // elts_[i]->get_elt_property ("Axis_group_element::add_extra_element") == SCM_UNDEFINED &&
	    ! dynamic_cast<Axis_group_element*> (elts_[i]->parent_l (Y_AXIS)))
	       
	{
	  staffline_p_->add_element (elts_[i]);
	}
    }
  elts_.clear ();
}

ADD_THIS_TRANSLATOR(Axis_group_engraver);
