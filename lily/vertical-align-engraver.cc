/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-col.hh"
#include "vertical-align-engraver.hh"
#include "axis-align-spanner.hh"
#include "axis-group-spanner.hh"

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
  Scalar dist (get_property ("maxVerticalAlign", 0));
  if (dist.length_i () && dist.isnum_b ())
    {
      valign_p_->threshold_interval_[BIGGER]  = Real (dist);
    }

  dist = get_property ("minVerticalAlign", 0);
  if (dist.length_i () && dist.isnum_b ())
    {
      valign_p_->threshold_interval_[SMALLER]  = Real (dist);
    }

  dist = get_property ("alignmentReference",0);
  if (dist.length_i () && dist.isnum_b ())
    {
      valign_p_->align_dir_ = int (dist);
    }
  valign_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (valign_p_);
  valign_p_ =0;
}

void
Vertical_align_engraver::acknowledge_element (Score_element_info i)
{
  if (i.origin_grav_l_arr_.size() == 1 &&
      dynamic_cast<Axis_group_spanner *> (i.elem_l_)
      && !i.elem_l_->parent_l (Y_AXIS))
    {
      valign_p_->add_element (i.elem_l_);
    }
}



ADD_THIS_TRANSLATOR(Vertical_align_engraver);
