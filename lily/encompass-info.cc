/*
  encompass-info.cc -- implement Encompass_info

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "stem.hh"
#include "note-column.hh"
#include "paper-def.hh"
#include "encompass-info.hh"
#include "slur.hh"
#include "staff-sym.hh"
#include "debug.hh"

Encompass_info::Encompass_info ()
{
  assert (0);
}

Encompass_info::Encompass_info (Note_column const* note, Direction dir)
{
  Paper_def* paper = note->paper ();
  Real interline = paper->interline_f ();
  // UGH
  Real notewidth = paper->note_width () * 0.8;
  Real internote = interline / 2;

  Stem* stem_l_ = note->stem_l_;
  /* 
    set o_.x () to middle of notehead or on the exact position of stem,
    according to slur direction
   */
  o_.x () = stem_l_->hpos_f ();

  /*
     stem_l_->dir == dir
                      ________
           |   |     /        \
          x|  x|       |x  |x
        \________/     |   |

   */

  if (stem_l_->dir_ != dir)
    o_.x () -= 0.5 * notewidth * stem_l_->dir_;

  o_.y () = stem_l_->extent (Y_AXIS)[dir];
  /*
   leave a gap: slur mustn't touch head/stem
   */
  o_.y () += 2.5 * internote * dir;

  if (stem_l_->dir_ != dir)
    o_.y () += 1.0 * internote * dir;

  Slur* slur_l_ = stem_l_->slur_l_;
  if (slur_l_->encompass_arr_.size ()
      && stem_l_->staff_sym_l_ != slur_l_->encompass_arr_[0]->stem_l_->staff_sym_l_)
    {
      if (stem_l_->staff_sym_l_->dim_cache_[Y_AXIS].valid_b ())
	{
	  slur_l_->interstaff_f_ = stem_l_->staff_sym_l_->absolute_coordinate (Y_AXIS)
	    - slur_l_->encompass_arr_[0]->stem_l_->staff_sym_l_->absolute_coordinate (Y_AXIS);
	}
      else
	{
	  warning (_ ("invalid dimension cache: guessing staff position"));
	  if (slur_l_->vertical_align_drul_[MIN] != 
	      slur_l_->vertical_align_drul_[MAX])
	    warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff slurs may be broken"));
	  slur_l_->interstaff_f_ = slur_l_->vertical_align_drul_[MIN];
	  // urg, guess staff order:
	  // if our stem ends higher, our staff is probably lower...
	  if (stem_l_->chord_start_f () > slur_l_->encompass_arr_[0]->stem_l_->chord_start_f ())
	    slur_l_->interstaff_f_ *= -1;
	}
      o_.y () += slur_l_->interstaff_f_;
    }
}
