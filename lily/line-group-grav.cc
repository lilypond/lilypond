/*
  staff-gravs.cc -- implement Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-sym.hh"
#include "vertical-group-spanner.hh"
#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "line-group-grav.hh"
#include "p-col.hh"

Line_group_engraver::Line_group_engraver()
{
  staffline_p_ =0;
}

void
Line_group_engraver::acknowledge_element (Score_elem_info  elem)
{
  if (!elem.elem_l_->axis_group_l_a_[Y_AXIS])
    staffline_p_->add_element (elem.elem_l_);
}


void
Line_group_engraver::do_removal_processing()
{
  staffline_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (staffline_p_);
  staffline_p_ = 0;
}

void
Line_group_engraver::do_creation_processing()
{
  create_line_spanner ();
  staffline_p_->set_bounds(LEFT,get_staff_info().command_pcol_l ());

  // don't broadcast to self.
  announce_element (Score_elem_info (staffline_p_,0));
}

void
Line_group_engraver::create_line_spanner ()
{
  staffline_p_ = new Vertical_group_spanner ;
}



IMPLEMENT_IS_TYPE_B1(Line_group_engraver,Engraver);
ADD_THIS_TRANSLATOR(Line_group_engraver);

