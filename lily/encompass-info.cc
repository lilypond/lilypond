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

  if (slur_l->encompass_arr_.size ()
      && stem_l->staff_symbol_l () != slur_l->encompass_arr_[0]->stem_l_->staff_symbol_l ())
    {
      if (slur_l->vertical_align_drul_[MIN] != 
	  slur_l->vertical_align_drul_[MAX])
	warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff slurs may be broken"));
      interstaff_f_ = slur_l->vertical_align_drul_[MIN];
      /* urg, guess staff order */
      int d = note->head_l_arr_.top ()->position_i_
	- slur_l->encompass_arr_[0]->head_l_arr_[0]->position_i_;
      if (abs (d > 3))
	interstaff_f_ *= sign (d);
      else if (stem_l->chord_start_f () >
	       slur_l->encompass_arr_[0]->stem_l_->chord_start_f ())
	interstaff_f_ *= -1;
      o_.y () += interstaff_f_;
    }
}
