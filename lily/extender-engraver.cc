/*
  extender-engraver.cc -- implement Extender_engraver

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "extender-engraver.hh"
#include "extender-spanner.hh"
#include "text-def.hh"
#include "score-column.hh"
#include "text-item.hh"

ADD_THIS_TRANSLATOR (Extender_engraver);

Extender_engraver::Extender_engraver ()
{
  extender_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

void
Extender_engraver::acknowledge_element (Score_element_info i)
{
  Text_item* t = (dynamic_cast<Text_item*> (i.elem_l_));
  if (!t)
    return;
  if (!extender_spanner_p_)
    return;
  if (!span_reqs_drul_[LEFT])
    return;

  if (!extender_spanner_p_->textitem_l_drul_[LEFT])
    extender_spanner_p_->set_textitem (LEFT, t);
  else
    if (span_reqs_drul_[RIGHT] && !extender_spanner_p_->textitem_l_drul_[RIGHT]) 
      extender_spanner_p_->set_textitem (RIGHT, t);
}

bool
Extender_engraver::do_try_music (Music* req_l)
{
  if (Extender_req* p = dynamic_cast <Extender_req *> (req_l))
    {
      if (bool (extender_spanner_p_) == bool (p->spantype == Span_req::START))
	return false;

      Direction d = (!extender_spanner_p_) ? LEFT : RIGHT;
      if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (p))
	return false;

      span_reqs_drul_[d] = p;
      span_mom_drul_[d] = get_staff_info ().musical_l ()->when ();
      return true;
    }
  return false;
}

void
Extender_engraver::do_removal_processing ()
{
  if (extender_spanner_p_)
    {
      span_reqs_drul_[LEFT]->warning (_ ("unterminated extender"));
      extender_spanner_p_->unlink ();
      delete extender_spanner_p_;
      extender_spanner_p_ = 0;
      span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
    }
}

void
Extender_engraver::do_process_requests ()
{
  if (extender_spanner_p_ || !span_reqs_drul_[LEFT])
    return;

  extender_spanner_p_ = new Extender_spanner;

  announce_element (Score_element_info (extender_spanner_p_, span_reqs_drul_[LEFT]));
}

void
Extender_engraver::do_pre_move_processing ()
{
  if (!extender_spanner_p_ || !span_reqs_drul_[RIGHT]) 
    return;

  typeset_element (extender_spanner_p_);
  
  extender_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

