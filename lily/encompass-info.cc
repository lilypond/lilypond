/*
  encompass-info.cc -- implement Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "stem.hh"
#include "note-column.hh"
#include "paper-def.hh"
#include "encompass-info.hh"
#include "slur.hh"
#include "staff-symbol.hh"
#include "note-head.hh"
#include "debug.hh"
#include "align-element.hh"

Encompass_info::Encompass_info ()
{
  assert (0);
}

Encompass_info::Encompass_info (Note_column const* note, Direction dir, Slur const* slur_l)
{
  interstaff_f_ = 0;
  
  Paper_def* paper = note->paper_l ();
  
  // UGH
  Real notewidth = paper->note_width () * 0.8;
  

  Stem* stem_l = note->stem_l_;
  Real internote = stem_l-> staff_line_leading_f ()/2.;

  /* 
    set o_.x () to middle of notehead or on the exact position of stem,
    according to slur direction
   */
  o_.x () = stem_l->hpos_f ();

  /*
     stem_l->dir == dir
                      ________
           |   |     /        \
          x|  x|       |x  |x
        \________/     |   |

   */

  if (stem_l->dir_ != dir)
    o_.x () -= 0.5 * notewidth * stem_l->dir_;

  o_.y () = stem_l->extent (Y_AXIS)[dir];
  /*
   leave a gap: slur mustn't touch head/stem
   */
  o_.y () += 2.5 * internote * dir;

  if (stem_l->dir_ != dir)
    o_.y () += 1.0 * internote * dir;


  Dimension_cache *common = note->common_group (slur_l, Y_AXIS);
  Align_element * align = dynamic_cast<Align_element*> (common->element_l ());
  if (align && align->axis() == Y_AXIS)
    {
      if (align->threshold_interval_[MIN] != 
	  align->threshold_interval_[MAX])
	warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff beams/slurs may be broken"));

      interstaff_f_ = align->threshold_interval_[MIN];

      Dimension_cache * slur_refpoint = &slur_l->dim_cache_[Y_AXIS];
      Dimension_cache * note_refpoint = &note->dim_cache_[Y_AXIS];

      while (slur_refpoint->parent_l_ != common)
	slur_refpoint = slur_refpoint->parent_l_;
      while (note_refpoint->parent_l_ != common)
	note_refpoint = note_refpoint->parent_l_;


      int slur_prio =
	align->get_priority (dynamic_cast<Score_element*> (slur_refpoint->element_l ()));
      int stem_prio =
	align->get_priority (dynamic_cast<Score_element*> (note_refpoint->element_l ()));

      /*
	our staff is lower -> interstaff_f_ *= -1
       */
      // ? Is this OK?
      if (slur_prio < stem_prio)
	interstaff_f_ *= -1;
      o_.y () += interstaff_f_;
    }
}
