/*
  beam-grav.cc -- implement Beam_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "duration-convert.hh"
#include "time-description.hh"
#include "beam-grav.hh"
#include "stem.hh"
#include "beam.hh"
#include "musical-request.hh"
#include "grouping.hh"
#include "text-spanner.hh"
#include "text-def.hh"

Beam_engraver::Beam_engraver()
{
  span_reqs_drul_[LEFT] = span_reqs_drul_[RIGHT] =0;
  beam_p_ =0;
  current_grouping_p_ =0;
}

bool
Beam_engraver::do_try_request(Request*r)
{
  Musical_req* mus_l = r->musical();
  if (!mus_l)
    return false;

  Beam_req * b = mus_l->beam();

  if (!b)
    return false;

  if (bool (beam_p_) == bool (b->spantype == Span_req::START))
    return false;

  Direction d = (!beam_p_) ? LEFT : RIGHT;
  if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (mus_l))
    return false;

  span_reqs_drul_[d] = b;
  return true;
}

void
Beam_engraver::do_process_requests()
{
  if ( !beam_p_ && span_reqs_drul_[LEFT]) {
    current_grouping_p_ = new Rhythmic_grouping;
    beam_p_ = new Beam;
    if (span_reqs_drul_[LEFT]->nplet)
      {
	Text_spanner* t = new Text_spanner();
	Text_def *defp = new Text_def;
	t->set_support (beam_p_);
	defp->align_i_ = 0;
	defp->text_str_ = span_reqs_drul_[LEFT]->nplet;
	defp->style_str_="italic";
	t->spec_p_  = defp;
	announce_element (Score_elem_info (t,0));
	typeset_element (t);
      }
    announce_element (Score_elem_info (beam_p_, span_reqs_drul_[LEFT]));
  }
}

void
Beam_engraver::do_pre_move_processing()
{
  if (beam_p_ && span_reqs_drul_[RIGHT]) {
    Rhythmic_grouping const * rg_C = get_staff_info().rhythmic_C_;
    rg_C->extend (current_grouping_p_->interval());
    beam_p_->set_grouping (*rg_C, *current_grouping_p_);
    typeset_element (beam_p_);
    beam_p_ = 0;

    delete current_grouping_p_;
    current_grouping_p_ = 0;

    span_reqs_drul_[RIGHT] =
      span_reqs_drul_[LEFT] = 0;
  }
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
Beam_engraver::acknowledge_element (Score_elem_info i)
{
  if (!beam_p_ || !i.elem_l_->is_type_b (Stem::static_name ()))
    return;

  Stem* s = (Stem*)i.elem_l_->item();
  Rhythmic_req *rhythmic_req = i.req_l_->musical ()->rhythmic ();
  if (rhythmic_req->duration_.durlog_i_<= 2)
    {
      rhythmic_req->warning (_("Stem doesn't fit in Beam"));
      return;
    }

  /*
    TODO: do something sensible if it doesn't fit in the beam.
   */
  current_grouping_p_->add_child (get_staff_info().time_C_->whole_in_measure_,
				  rhythmic_req->duration ());
  s->flag_i_ = rhythmic_req->duration_.durlog_i_;
  beam_p_->add (s);
}

IMPLEMENT_IS_TYPE_B1(Beam_engraver, Engraver);
ADD_THIS_TRANSLATOR(Beam_engraver);
