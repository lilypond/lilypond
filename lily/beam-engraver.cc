/*
  beam-grav.cc -- implement Beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "duration-convert.hh"
#include "time-description.hh"
#include "beam-engraver.hh"
#include "stem.hh"
#include "beam.hh"
#include "musical-request.hh"
#include "grouping.hh"
#include "p-col.hh"
#include "warn.hh"

Beam_engraver::Beam_engraver()
{
  span_reqs_drul_[LEFT] = span_reqs_drul_[RIGHT] =0;
  beam_p_ =0;
  current_grouping_p_ =0;
}

bool
Beam_engraver::do_try_music (Music*r)
{
  Beam_req* b = dynamic_cast <Beam_req *> (r);
  if (!b)
    return false;

  if (bool (beam_p_) == bool (b->spantype_ == START))
    return false;

  Direction d = (!beam_p_) ? LEFT : RIGHT;
  if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (b))
    return false;

  span_reqs_drul_[d] = b;
  return true;
}

void
Beam_engraver::do_process_requests()
{
  if (beam_p_ || !span_reqs_drul_[LEFT])
    return;

  current_grouping_p_ = new Rhythmic_grouping;
  beam_p_ = new Beam;

  Scalar prop = get_property ("beamslopedamping");
  if (prop.isnum_b ()) 
    beam_p_->damping_i_ = prop;

  prop = get_property ("beamquantisation");
  if (prop.isnum_b ()) 
    beam_p_->quantisation_ = (Beam::Quantisation)(int)prop;
 
  announce_element (Score_element_info (beam_p_, span_reqs_drul_[LEFT]));
}

void
Beam_engraver::do_pre_move_processing()
{
  if (!beam_p_ || !span_reqs_drul_[RIGHT]) 
    return;

  Rhythmic_grouping const * rg_C = get_staff_info().rhythmic_C_;
  rg_C->extend (current_grouping_p_->interval());
  beam_p_->set_grouping (*rg_C, *current_grouping_p_);
  typeset_element (beam_p_);
  beam_p_ = 0;

  delete current_grouping_p_;
  current_grouping_p_ = 0;

  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

void
Beam_engraver::do_removal_processing()
{
  if (beam_p_)
    {
      span_reqs_drul_[LEFT]->warning (_("unterminated beam"));
      typeset_element (beam_p_);
      beam_p_ =0;
    }
}


void
Beam_engraver::acknowledge_element (Score_element_info i)
{
  Stem* s = dynamic_cast<Stem *> (i.elem_l_);
  if (!beam_p_ || !s)
    return;

  if (!dynamic_cast <Rhythmic_req *> (i.req_l_))
    {
      ::warning ( _("Stem must have Rhythmic structure."));
      return;
    }

  Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (i.req_l_);
  if (rhythmic_req->duration_.durlog_i_<= 2)
    {
      rhythmic_req->warning (_ ("stem doesn't fit in beam"));
      return;
    }

  /*
    TODO: do something sensible if it doesn't fit in the beam.
   */
  Moment start = get_staff_info().time_C_->whole_in_measure_;

  if (!current_grouping_p_->child_fit_b (start))
    {
      String s (_("please fix me") + ": " 
	+ _f ("stem at %s doesn't fit in beam", now_moment ().str ()));
      if (i.req_l_)
	i.req_l_->warning(s);
      else 
	warning (s);
    }
  else
    {
      current_grouping_p_->add_child (start, rhythmic_req->duration ());
      s->flag_i_ = rhythmic_req->duration_.durlog_i_;
      beam_p_->add_stem (s);
    }
}

ADD_THIS_TRANSLATOR(Beam_engraver);
