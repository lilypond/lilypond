/*
  stem-info.cc -- implement Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "misc.hh"
#include "debug.hh"

#include "align-element.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"
#include "beam.hh"
#include "staff-symbol.hh"

Stem_info::Stem_info ()
{
}
/*
  FIXME: y dims should not be in internote.
 */

Stem_info::Stem_info (Stem*s, int mult)
{
  mult_i_ =mult;
  stem_l_ = s;
  x_ = stem_l_->hpos_f ();
  dir_ = stem_l_->dir_;
  SCM bd = stem_l_->remove_elt_property (beam_dir_scm_sym);
  
  beam_dir_ = gh_scm2int (SCM_CDR(bd));
  interstaff_f_ = 0;

  Paper_def* paper_l = stem_l_->paper_l ();
  Real internote_f = stem_l_->staff_line_leading_f ()/2;
  Real interbeam_f = paper_l->interbeam_f (mult_i_);
  Real beam_f = paper_l->beam_thickness_f ();
         
  {
      static int i = 1;
      DOUT << "******" << i++ << "******\n" 
	   << "begin_f: " << stem_l_->stem_begin_f () * dir_ 
	   << "\nchord_f/i: " << stem_l_->chord_start_f () * dir_ / internote_f << '\n';
  }

  // strangely enough, dim(chord_start_f) == pt (and not internote!)
  idealy_f_ = stem_l_->chord_start_f () / internote_f;

  // calculate using dim(y) == pt
  idealy_f_ *= internote_f;

  // for simplicity, we calculate as if dir == UP
  idealy_f_ *= beam_dir_;
  
  int stem_max = (int)rint(paper_l->get_var ("stem_max"));
  Real min_stem_f = paper_l->get_var (String ("minimum_stem_length")
				     + to_str (mult_i_ <? stem_max));
  Real stem_f = paper_l->get_var (String ("stem_length")
				 + to_str (mult_i_ <? stem_max));

  if (!beam_dir_ || (beam_dir_ == dir_))
    /* normal beamed stem */
    {
      if (mult_i_)
	{
	  idealy_f_ += beam_f;
	  idealy_f_ += (mult_i_ - 1) * interbeam_f;
	}
      miny_f_ = idealy_f_;
      maxy_f_ = INT_MAX;

      idealy_f_ += stem_f;
      miny_f_ += min_stem_f;

      // lowest beam of (UP) beam must never be lower than second staffline
      miny_f_ = miny_f_ >? (- 2 * internote_f - beam_f
	+ (mult_i_ > 0) * beam_f + interbeam_f * (mult_i_ - 1));
    }
  else
    /* knee */
    {
      idealy_f_ -= beam_f;
      // idealy_f_ -= (mult_i_ - 1) * interbeam_f;
      // idealy_f_ += (mult_i_ - stem_l_->flag_i_ >? 0) * interbeam_f;
      maxy_f_ = idealy_f_;
      miny_f_ = -INT_MAX;

      idealy_f_ -= stem_f;
      maxy_f_ -= min_stem_f;
    }

  // set dim(y) == internote
  idealy_f_ /= internote_f;
  miny_f_ /= internote_f;
  maxy_f_ /= internote_f;

  DOUT << "dir_: " << dir_ << '\n';
  DOUT << "mult_i_: " << mult_i_ << '\n';
  DOUT << "idealy_f_: " << idealy_f_ << '\n';
  DOUT << "miny_f_: " << miny_f_ << '\n';
  DOUT << "maxy_f_: " << maxy_f_ << '\n';

  idealy_f_ = maxy_f_ <? idealy_f_;
  idealy_f_ = miny_f_ >? idealy_f_;

  // interstaff beam
  Beam* beam_l = stem_l_->beam_l_;
  
  Dimension_cache *common = stem_l_->common_group (beam_l, Y_AXIS);
  Align_element * align = dynamic_cast<Align_element*> (common->element_l ());
  if (align && align->axis() == Y_AXIS)
    {
      if (align->threshold_interval_[MIN] != 
	  align->threshold_interval_[MAX])
	warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff beams/slurs may be broken"));

      interstaff_f_ = align->threshold_interval_[MIN] / internote_f;

      Dimension_cache * beam_refpoint = beam_l->dim_cache_[Y_AXIS];
      Dimension_cache * stem_refpoint = stem_l_->dim_cache_[Y_AXIS];

      while (beam_refpoint->parent_l_ != common)
	beam_refpoint = beam_refpoint->parent_l_;
      while (stem_refpoint->parent_l_ != common)
	stem_refpoint = stem_refpoint->parent_l_;


      int beam_prio =
	align->get_priority (dynamic_cast<Score_element*> (beam_refpoint->element_l ()));
      int stem_prio =
	align->get_priority (dynamic_cast<Score_element*> (stem_refpoint->element_l ()));

      /*
	our staff is lower -> interstaff_f_ *= -1
       */
      if (beam_prio < stem_prio)
	interstaff_f_ *= -1;
      
      idealy_f_ += interstaff_f_ * beam_dir_;
      miny_f_ += interstaff_f_ * beam_dir_;
      maxy_f_ += interstaff_f_ * beam_dir_;
    }
}

