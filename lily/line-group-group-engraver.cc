/*
  staff-gravs.cc -- implement Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-symbol.hh"
#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "line-group-group-engraver.hh"
#include "paper-column.hh"
#include "axis-group-interface.hh"


Line_group_engraver_group::Line_group_engraver_group()
{
  staffline_p_ =0;
}


void
Line_group_engraver_group::typeset_element (Score_element *elem)
{
  if (!elem->parent_l (Y_AXIS))      
    Axis_group_interface (staffline_p_).add_element (elem);
  Engraver_group_engraver::typeset_element (elem);
}


void
Line_group_engraver_group::do_removal_processing()
{
  Engraver_group_engraver::do_removal_processing ();
  Item *  it = get_staff_info().command_pcol_l ();

  Pointer_group_interface (it, "bounded-by-me").add_element (staffline_p_);  
  staffline_p_->set_bound(RIGHT,it);
  Engraver_group_engraver::typeset_element (staffline_p_);
  staffline_p_ = 0;
}

void
Line_group_engraver_group::do_creation_processing()
{
  create_line_spanner ();
  Item *  it = get_staff_info().command_pcol_l ();  
  staffline_p_->set_bound(LEFT,it);
  Pointer_group_interface (it, "bounded-by-me").add_element (staffline_p_);
  
  Engraver::announce_element (Score_element_info (staffline_p_,0));
}

void
Line_group_engraver_group::create_line_spanner ()
{
  staffline_p_ = new Spanner (SCM_EOL);
  Axis_group_interface (staffline_p_).set_interface ();
  Axis_group_interface (staffline_p_).set_axes (Y_AXIS,Y_AXIS);
}




ADD_THIS_TRANSLATOR(Line_group_engraver_group);

