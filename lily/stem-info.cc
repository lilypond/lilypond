/*
  stem-info.cc -- implement Stem_info

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "proto.hh"
#include "misc.hh"
#include "cross-staff.hh"
#include "debug.hh"
#include "stem.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "stem-info.hh"
#include "beam.hh"


Stem_info::Stem_info ()
{
}


/*
  FIXME: y dims should not be in internote.


  GURG UGRGINRG INA UG R

  JUNKME -> This should be in Beam
*/
Stem_info::Stem_info (Stem*s, int mult)
{
  mult_i_ =mult;
  stem_l_ = s;
  Beam* beam_l = stem_l_->beam_l_;
  
  x_ = stem_l_->hpos_f ();
  set_direction (stem_l_->get_direction ());
  SCM bd = stem_l_->remove_elt_property ("beam-dir");
  
  if (gh_number_p (bd))
    {
      beam_dir_ = gh_scm2int (bd);
    }
  else
    {
      programming_error ("Beam direction not set."); 
      beam_dir_ = UP;		//  GURAUGRNAGURAGU! urg !
    }
  
  Paper_def* paper_l = stem_l_->paper_l ();
  Real internote_f = stem_l_->staff_line_leading_f ()/2;
  Real interbeam_f = paper_l->interbeam_f (mult_i_);
  Real beam_f = gh_scm2double (beam_l->get_elt_property ("beam-thickness"));
         

  // strangely enough, dim(chord_start_f) == pt (and not internote!)
  idealy_f_ = stem_l_->chord_start_f () / internote_f;

  // calculate using dim(y) == pt
  idealy_f_ *= internote_f;

  // for simplicity, we calculate as if dir == UP
  idealy_f_ *= beam_dir_;

  bool grace_b = stem_l_->get_elt_property ("grace") != SCM_UNDEFINED;
  bool no_extend_b = stem_l_->get_elt_property ("no-stem-extend") 
    != SCM_UNDEFINED;

  int stem_max = (int)rint(paper_l->get_var ("stem_max"));
  String type_str = grace_b ? "grace_" : "";
  Real min_stem_f = paper_l->get_var (type_str + "minimum_stem_length"
				      + to_str (mult_i_ <? stem_max)) * internote_f;
  Real stem_f = paper_l->get_var (type_str + "stem_length"
				  + to_str (mult_i_ <? stem_max))* internote_f;

  if (!beam_dir_ || (beam_dir_ == get_direction ()))
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

      /*
	lowest beam of (UP) beam must never be lower than second staffline

	Hmm, reference (Wanske?)

	Although this (additional) rule is probably correct,
	I expect that highest beam (UP) should also never be lower
	than middle staffline, just as normal stems.
	
      */
      if (!grace_b && !no_extend_b)
	{
	  //highest beam of (UP) beam must never be lower than middle staffline
	  miny_f_ = miny_f_ >? 0;
	  //lowest beam of (UP) beam must never be lower than second staffline
	  miny_f_ = miny_f_ >? (- 2 * internote_f - beam_f
				+ (mult_i_ > 0) * beam_f + interbeam_f * (mult_i_ - 1));
	}
    }
  else
    /* knee */
    {
      idealy_f_ -= beam_f;
      maxy_f_ = idealy_f_;
      miny_f_ = -INT_MAX;

      idealy_f_ -= stem_f;
      maxy_f_ -= min_stem_f;
    }

  // set dim(y) == internote
  idealy_f_ /= internote_f;
  miny_f_ /= internote_f;
  maxy_f_ /= internote_f;

  idealy_f_ = maxy_f_ <? idealy_f_;
  idealy_f_ = miny_f_ >? idealy_f_;

  interstaff_f_ = calc_interstaff_dist (stem_l_, beam_l) / internote_f;
  idealy_f_ += interstaff_f_* beam_dir_;
  miny_f_   += interstaff_f_ * beam_dir_;
  maxy_f_   += interstaff_f_ * beam_dir_;
}


