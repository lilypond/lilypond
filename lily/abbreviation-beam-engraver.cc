/*
  abbreviation-beam-engraver.cc -- implement Abbreviation_beam_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "duration-convert.hh"
#include "time-description.hh"
#include "abbreviation-beam-engraver.hh"
#include "stem.hh"
#include "abbreviation-beam.hh"
#include "musical-request.hh"
#include "misc.hh"

IMPLEMENT_IS_TYPE_B1(Abbreviation_beam_engraver, Engraver);
ADD_THIS_TRANSLATOR(Abbreviation_beam_engraver);

Abbreviation_beam_engraver::Abbreviation_beam_engraver ()
{
  span_reqs_drul_[LEFT] = span_reqs_drul_[RIGHT] = 0;
  abeam_p_ = 0;
}

bool
Abbreviation_beam_engraver::do_try_request (Request*r)
{
  Musical_req* mus_l = r->access_Musical_req ();
  if (!mus_l)
    return false;

  Abbreviation_beam_req * b = mus_l->access_Abbreviation_beam_req ();

  if (!b)
    return false;

  if (bool (abeam_p_) == bool (b->spantype == Span_req::START))
    return false;

  Direction d = (!abeam_p_) ? LEFT : RIGHT;
  if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (mus_l))
    return false;

  span_reqs_drul_[d] = b;
  return true;
}

void
Abbreviation_beam_engraver::do_process_requests ()
{
  if (!abeam_p_ && span_reqs_drul_[LEFT]) {
    abeam_p_ = new Abbreviation_beam;
    announce_element (Score_element_info (abeam_p_, span_reqs_drul_[LEFT]));
  }
}

void
Abbreviation_beam_engraver::do_pre_move_processing ()
{
  if (abeam_p_ && span_reqs_drul_[RIGHT]) {
    typeset_element (abeam_p_);
    abeam_p_ = 0;

    span_reqs_drul_[RIGHT] =
      span_reqs_drul_[LEFT] = 0;
  }
}

void
Abbreviation_beam_engraver::do_removal_processing ()
{
  if (abeam_p_)
    {
      span_reqs_drul_[LEFT]->warning (_("unterminated beam"));
      typeset_element (abeam_p_);
      abeam_p_ = 0;
    }
}

void
Abbreviation_beam_engraver::acknowledge_element (Score_element_info i)
{
  if (!abeam_p_ || !i.elem_l_->is_type_b (Stem::static_name ()))
    return;

  Stem* s = (Stem*)i.elem_l_->access_Item ();

  int type_i = span_reqs_drul_[LEFT]->type_i_;
  s->flag_i_ = intlog2 (type_i) - 2;
  if (span_reqs_drul_[RIGHT])
    s->beams_left_i_ = s->flag_i_;
  else
    s->beams_right_i_ = s->flag_i_;

  abeam_p_->multiple_i_ = s->flag_i_;
  if (s->type_i () != 1) // no abbrev gaps on half note
    s->beam_gap_i_ = s->flag_i_ - ((s->type_i () >? 2) - 2);

  abeam_p_->add_stem (s);
}
