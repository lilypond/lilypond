/*
  stem-info.cc -- implement Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "misc.hh"
#include "debug.hh"
#include "atom.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"

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

  /*
    [TODO]
    make this runtime

    Breitkopf + H\"artel:
    miny_f_ = interline + #beams * interbeam
    ideal8 = 2 * interline + interbeam
    ideal16,32,64,128 = 1.5 * interline + #beams * interbeam

    * B\"arenreiter:
    miny_f_ = interline + #beams * interbeam
    ideal8,16 = 2 interline + #beams * interbeam
    ideal32,64,128 = 1.5 interline + #beams * interbeam
       
    */

  Real internote_f = stem_l_->paper ()->internote_f ();
  Real interbeam_f = stem_l_->paper ()->interbeam_f (mult_i_);
  Real beam_f = stem_l_->paper ()->beam_thickness_f ();
         

  {
      static int i = 1;
      DOUT << "******" << i++ << "******\n" 
	   << "begin_f: " << stem_l_->stem_begin_f () * dir_ 
	   << "\nchord_f/i: " << stem_l_->chord_start_f () * dir_ / internote_f << '\n';
  }

  /*
    For simplicity, we'll assume dir = UP and correct if 
    dir = DOWN afterwards.
   */
  idealy_f_ = stem_l_->chord_start_f () * beam_dir_ / internote_f;
  idealy_f_ *= internote_f;

  Real break_i = (int)rint (stem_l_->paper ()->get_var ("beam_multiple_break"));
  Real min_stem1_f = stem_l_->paper ()->get_var ("beam_minimum_stem1");
  Real min_stem2_f = stem_l_->paper ()->get_var ("beam_minimum_stem2");
  Real ideal_stem1_f = stem_l_->paper ()->get_var ("beam_ideal_stem1");
  Real ideal_stem2_f = stem_l_->paper ()->get_var ("beam_ideal_stem2");
  Real shorten_f = stem_l_->paper ()->get_var ("forced_stem_shorten");

  if (!beam_dir_ || (beam_dir_ == dir_))
    /* normal (beamed) stem */
    {
      idealy_f_ += interbeam_f * mult_i_;
      miny_f_ = idealy_f_;
      maxy_f_ = INT_MAX;

      if (mult_i_ < break_i)
        {
	  idealy_f_ += ideal_stem1_f;
	  miny_f_ += min_stem1_f;
	}
      else
        {
	  idealy_f_ += ideal_stem2_f;
	  miny_f_ += min_stem2_f;
	}

      /*
        stems in unnatural (forced) direction are shortened but
        - central line is never 'forced'
        - beamed stems are shortened only by beam itself
       */
      if (!mult_i_ && ((int)stem_l_->chord_start_f ()) && (stem_l_->dir_ != stem_l_->get_default_dir ()))
 	{
	  idealy_f_ -= shorten_f;
	}

      // lowest beam of (UP) beam must never be lower than second staffline
      miny_f_ = miny_f_ >? (- 2 * internote_f - beam_f
	+ (mult_i_ > 0) * beam_f + interbeam_f * (mult_i_ - 1));
    }
  else
    /* knee */
    {
      idealy_f_ -= beam_f;
      maxy_f_ = idealy_f_;
      miny_f_ = -INT_MAX;

      if (mult_i_ < break_i)
        {
	  idealy_f_ -= ideal_stem1_f;
	  maxy_f_ -= min_stem1_f;
	}
      else
        {
	  idealy_f_ -= ideal_stem2_f;
	  maxy_f_ -= min_stem2_f;
	}
    }


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
}

