/*
  vertical-align-grav.cc -- implement Vertical_align_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "p-col.hh"
#include "vertical-align-engraver.hh"
#include "vertical-align-spanner.hh"
#include "vertical-group-spanner.hh"

Vertical_align_engraver::Vertical_align_engraver()
{
  valign_p_ =0;
}

void
Vertical_align_engraver::do_creation_processing()
{
  valign_p_ =new Vertical_align_spanner;
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
  
  valign_p_->set_bounds(RIGHT,get_staff_info().command_pcol_l ());
  typeset_element (valign_p_);
  valign_p_ =0;
}

void
Vertical_align_engraver::acknowledge_element (Score_element_info i)
{
  if (i.origin_grav_l_arr_.size() == 1
      && dynamic_cast<Vertical_group_spanner *> (i.elem_l_)
      && !i.elem_l_->parent_l (Y_AXIS))
    {
      assert (!valign_p_->contains_b (i.elem_l_));

      valign_p_->add_element (i.elem_l_);
    }
}



ADD_THIS_TRANSLATOR(Vertical_align_engraver);
