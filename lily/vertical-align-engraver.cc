/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "translator-group.hh"
#include "axis-group-engraver.hh"
#include "paper-column.hh"
#include "vertical-align-engraver.hh"
#include "axis-align-spanner.hh"
#include "axis-group-spanner.hh"
#include "span-bar.hh"

Vertical_align_engraver::Vertical_align_engraver()
{
  valign_p_ =0;
}

void
Vertical_align_engraver::do_creation_processing()
{
  valign_p_ =new Axis_align_spanner;
  valign_p_->set_axis (Y_AXIS);
  valign_p_->stacking_dir_ = DOWN;
  
  valign_p_->set_bounds(LEFT,get_staff_info().command_pcol_l ());
  announce_element (Score_element_info (valign_p_ , 0));
}

void
Vertical_align_engraver::do_removal_processing()
{
  SCM dist (get_property ("maxVerticalAlign", 0));
  if (gh_number_p(dist))
    {
      valign_p_->threshold_interval_[BIGGER]  = gh_scm2double (dist);
    }

  dist = get_property ("minVerticalAlign", 0);
  if (gh_number_p(dist))
    {
      valign_p_->threshold_interval_[SMALLER]  = gh_scm2double (dist);
    }

  dist = get_property ("alignmentReference",0);
  if (isdir_b (dist))
    {
      valign_p_->align_dir_ = to_dir (dist);
    }
  valign_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (valign_p_);
  valign_p_ =0;
}


bool
Vertical_align_engraver::qualifies_b (Score_element_info i) const
{
  int sz = i.origin_trans_l_arr_.size()  ;


  Axis_group_element * elt = dynamic_cast<Axis_group_element *> (i.elem_l_);

  return sz > 1 && elt && elt->axes_[0] == Y_AXIS && !elt->parent_l (Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_element (Score_element_info i)
{
  if (qualifies_b (i))
    {
      valign_p_->add_element (i.elem_l_);
    }
  else if (dynamic_cast<Span_bar*>(i.elem_l_) && i.origin_trans_l_arr_.size ())
    {
      i.elem_l_->add_dependency (valign_p_);
    }  
}



ADD_THIS_TRANSLATOR(Vertical_align_engraver);
