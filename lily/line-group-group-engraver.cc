/*
  staff-gravs.cc -- implement Line_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "staff-sym.hh"
#include "vertical-group-spanner.hh"
#include "command-request.hh"
#include "bar.hh"
#include "debug.hh"
#include "line-group-group-engraver.hh"
#include "p-col.hh"

Line_group_engraver_group::Line_group_engraver_group()
{
  staffline_p_ =0;
}


void
Line_group_engraver_group::typeset_element (Score_element *elem)
{
  if (!elem->parent_l (Y_AXIS))      
    staffline_p_->add_element (elem);
  Engraver_group_engraver::typeset_element (elem);
}


void
Line_group_engraver_group::do_removal_processing()
{
  Engraver_group_engraver::do_removal_processing ();

  staffline_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  Engraver_group_engraver::typeset_element (staffline_p_);
  staffline_p_ = 0;
}

void
Line_group_engraver_group::do_creation_processing()
{
  create_line_spanner ();
  staffline_p_->set_bounds(LEFT,get_staff_info().command_pcol_l ());
  
  Engraver::announce_element (Score_element_info (staffline_p_,0));
}

void
Line_group_engraver_group::create_line_spanner ()
{
  staffline_p_ = new Vertical_group_spanner ;
}

void
Line_group_engraver_group::do_announces ()
{
  Engraver_group_engraver::do_announces ();
}



ADD_THIS_TRANSLATOR(Line_group_engraver_group);

