/*
  plet-engraver.cc -- implement Plet_engraver

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "plet-engraver.hh"
#include "plet-spanner.hh"
#include "text-def.hh"
#include "stem.hh"

IMPLEMENT_IS_TYPE_B1 (Plet_engraver,Engraver);
ADD_THIS_TRANSLATOR (Plet_engraver);

Plet_engraver::Plet_engraver ()
{
  plet_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

void
Plet_engraver::acknowledge_element (Score_elem_info i)
{
  if (!i.elem_l_->is_type_b (Stem::static_name ()))
    return;
  if (!plet_spanner_p_)
    return;
  if (!span_reqs_drul_[LEFT])
    return;

  if (!plet_spanner_p_->stem_l_drul_[LEFT])
    plet_spanner_p_->set_stem (LEFT, (Stem*)i.elem_l_->item ());
  else
    if (span_reqs_drul_[RIGHT] && !plet_spanner_p_->stem_l_drul_[RIGHT]) 
      plet_spanner_p_->set_stem (RIGHT, (Stem*)i.elem_l_->item());
}

bool
Plet_engraver::do_try_request (Request* req_l)
{
  Musical_req* mus_l = req_l->musical ();
  if (!mus_l)
    return false;

  Plet_req* p = mus_l->plet ();
  if (!p)
    return false;

  if (bool (plet_spanner_p_) == bool (p->spantype == Span_req::START))
    return false;

  Direction d = (!plet_spanner_p_) ? LEFT : RIGHT;
  if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (mus_l))
    return false;

  span_reqs_drul_[d] = p;
  return true;
}

void
Plet_engraver::do_removal_processing ()
{
  if (plet_spanner_p_)
    {
      span_reqs_drul_[LEFT]->warning (_("unterminated plet"));
      typeset_element (plet_spanner_p_);
      plet_spanner_p_ = 0;
    }
}

void
Plet_engraver::do_process_requests ()
{
  if (plet_spanner_p_ || !span_reqs_drul_[LEFT])
    return;

  plet_spanner_p_ = new Plet_spanner;
  plet_spanner_p_->tdef_p_->text_str_ = span_reqs_drul_[LEFT]->plet_i_;

  announce_element (Score_elem_info (plet_spanner_p_, span_reqs_drul_[LEFT]));
}

void
Plet_engraver::do_pre_move_processing ()
{
  if (!plet_spanner_p_ || !span_reqs_drul_[RIGHT]) 
    return;

  typeset_element (plet_spanner_p_);
  plet_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

