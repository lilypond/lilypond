/*
  stem-info.cc -- implement Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "misc.hh"
#include "debug.hh"

#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"
#include "beam.hh"
#include "staff-symbol.hh"

Stem_info::Stem_info ()
{
}

Stem_info::Stem_info (Stem*s)
{
  stem_l_ = s;
  x_ = stem_l_->hpos_f ();
  dir_ = stem_l_->dir_;
  beam_dir_ = stem_l_->beam_dir_;
  mult_i_ = stem_l_->mult_i_;
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
  Beam* beam_l_ = stem_l_->beam_l_;
  if (beam_l_->sinfo_.size ()
      && stem_l_->staff_symbol_l () != beam_l_->sinfo_[0].stem_l_->staff_symbol_l ())
    {
	{
	  //	  warning (_ ("invalid dimension cache: guessing staff position"));
	  if (beam_l_->vertical_align_drul_[MIN] != 
	      beam_l_->vertical_align_drul_[MAX])
	    warning (_ ("minVerticalAlign != maxVerticalAlign: interstaff slurs may be broken"));
	  interstaff_f_ = beam_l_->vertical_align_drul_[MIN] / internote_f;
	  // urg, guess staff order:
	  // if our stem ends higher, our staff is probably lower...
	  if (idealy_f_ * beam_dir_ > beam_l_->sinfo_[0].idealy_f_ * beam_dir_)
	    interstaff_f_ *= -1;
	}
      idealy_f_ += interstaff_f_ * beam_dir_;
      miny_f_ += interstaff_f_ * beam_dir_;
      maxy_f_ += interstaff_f_ * beam_dir_;
    }
}

