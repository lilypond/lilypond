/*
  line-group-engraver-group.cc -- implement Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "line-group-group-engraver.hh"
#include "paper-column.hh"
#include "axis-group-interface.hh"
#include "spanner.hh"


Line_group_engraver_group::Line_group_engraver_group ()
{
  staffline_p_ =0;
}


void
Line_group_engraver_group::typeset_grob (Grob *elem)
{
  if (!elem->get_parent (Y_AXIS))      
    Axis_group_interface::add_element (staffline_p_, elem);
  Engraver_group_engraver::typeset_grob (elem);
}


void
Line_group_engraver_group::finalize ()
{
  Engraver_group_engraver::finalize ();
  Grob *  it
    = unsmob_grob (get_property ("currentCommandColumn"));

  staffline_p_->set_bound (RIGHT,it);
  Engraver_group_engraver::typeset_grob (staffline_p_);
  staffline_p_ = 0;
}

void
Line_group_engraver_group::initialize ()
{
  create_line_spanner ();
  Grob *  it
    = unsmob_grob (get_property ("currentCommandColumn")); 
  staffline_p_->set_bound (LEFT,it);
  
  Engraver::announce_grob (staffline_p_, SCM_EOL);
}

void
Line_group_engraver_group::create_line_spanner ()
{
  staffline_p_ = new Spanner (SCM_EOL);
  Axis_group_interface::set_interface (staffline_p_);
  Axis_group_interface::set_axes (staffline_p_, Y_AXIS,Y_AXIS);
}

ENTER_DESCRIPTION(Line_group_engraver_group,
		  "",
		  "",
		  "",
		  "",
		  "");

