/*   
  axis-group-engraver.cc --  implement Axis_group_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "axis-group-engraver.hh"
#include "spanner.hh"
#include "paper-column.hh"
#include "axis-group-interface.hh"
#include "dimension-cache.hh"
#include "engraver-group-engraver.hh"

Axis_group_engraver::Axis_group_engraver ()
{
  staffline_p_ = 0;
}

void
Axis_group_engraver::do_creation_processing ()
{
  staffline_p_ = get_spanner_p ();
  Axis_group_interface (staffline_p_).set_interface ();
  Axis_group_interface (staffline_p_).set_axes (Y_AXIS, Y_AXIS);
  staffline_p_->set_bound(LEFT,get_staff_info().command_pcol_l ());
  announce_element (Score_element_info (staffline_p_, 0));
}

Spanner*
Axis_group_engraver::get_spanner_p () const
{
  return new Spanner;
}

void
Axis_group_engraver::do_removal_processing ()
{
  String type = daddy_grav_l ()->type_str_ ;
  SCM dims = get_property (type  + "VerticalExtent");
  
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    {
      staffline_p_->set_extent_callback (&Score_element::preset_extent, Y_AXIS);
      staffline_p_->set_elt_property ("extent-Y", dims);
    }

  dims = get_property (type + "MinimumVerticalExtent");
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    staffline_p_->set_elt_property ("minimum-extent-Y", dims);

  dims = get_property (type + "ExtraVerticalExtent");
  if (gh_pair_p (dims) && gh_number_p (gh_car (dims))
      && gh_number_p (gh_cdr (dims)))
    staffline_p_->set_elt_property ("extra-extent-Y", dims);

  
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
      Score_element *par = elts_[i]->parent_l (Y_AXIS);

      if ((!par || !Axis_group_interface (par).has_interface_b ())
	  && ! elts_[i]->empty_b (Y_AXIS))
	Axis_group_interface (staffline_p_).add_element (elts_[i]);
    }
  elts_.clear ();
}

ADD_THIS_TRANSLATOR(Axis_group_engraver);
