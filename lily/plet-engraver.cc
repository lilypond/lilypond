/*
  plet-engraver.cc -- implement Plet_engraver

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "plet-engraver.hh"
#include "plet-spanner.hh"
#include "text-def.hh"
#include "beam.hh"
#include "score-column.hh"
#include "stem.hh"

IMPLEMENT_IS_TYPE_B1 (Plet_engraver,Engraver);
ADD_THIS_TRANSLATOR (Plet_engraver);

Plet_engraver::Plet_engraver ()
{
  beam_mom_drul_[LEFT] = span_mom_drul_[LEFT] = INT_MAX;
  beam_mom_drul_[RIGHT] = span_mom_drul_[LEFT] = -INT_MAX;
  plet_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

void
Plet_engraver::acknowledge_element (Score_element_info i)
{
  if (!i.elem_l_->is_type_b (Stem::static_name ()))
    return;
  if (!plet_spanner_p_)
    return;
  if (!span_reqs_drul_[LEFT])
    return;

  if (!plet_spanner_p_->stem_l_drul_[LEFT])
    plet_spanner_p_->set_stem (LEFT, (Stem*)i.elem_l_->access_Item ());
  else
    if (span_reqs_drul_[RIGHT] && !plet_spanner_p_->stem_l_drul_[RIGHT]) 
      plet_spanner_p_->set_stem (RIGHT, (Stem*)i.elem_l_->access_Item ());
}

bool
Plet_engraver::do_try_request (Request* req_l)
{
  Musical_req* mus_l = req_l->access_Musical_req ();
  if (!mus_l)
    return false;

  /*
    UGH! This is incorrect!
    Beam_req might not reach the Plet_engraver if ordering is wrong!
   */
  Beam_req* b = mus_l->access_Beam_req ();
  if (b)
    {
      if (b->spantype)
        {
          Direction d = (Direction)(((int)(b->spantype - 1)) * 2 - 1);
          beam_mom_drul_[d] = get_staff_info ().musical_l ()->when ();
	}
      return false;
    }
    
  Plet_req* p = mus_l->access_Plet_req ();
  if (!p)
    return false;

  if (bool (plet_spanner_p_) == bool (p->spantype == Span_req::START))
    return false;

  Direction d = (!plet_spanner_p_) ? LEFT : RIGHT;
  if (span_reqs_drul_[d] && !span_reqs_drul_[d]->equal_b (mus_l))
    return false;

  span_reqs_drul_[d] = p;
  span_mom_drul_[d] = get_staff_info ().musical_l ()->when ();
  return true;
}

void
Plet_engraver::do_removal_processing ()
{
  if (plet_spanner_p_)
    {
      span_reqs_drul_[LEFT]->warning (_ ("unterminated plet"));
      plet_spanner_p_->unlink ();
      delete plet_spanner_p_;
      plet_spanner_p_ = 0;
      span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
    }
}

void
Plet_engraver::do_process_requests ()
{
  if (plet_spanner_p_ || !span_reqs_drul_[LEFT])
    return;

  plet_spanner_p_ = new Plet_spanner;
  plet_spanner_p_->tdef_p_->text_str_ = to_str (span_reqs_drul_[LEFT]->plet_i_);

  announce_element (Score_element_info (plet_spanner_p_, span_reqs_drul_[LEFT]));
}

void
Plet_engraver::do_pre_move_processing ()
{
  if (!plet_spanner_p_ || !span_reqs_drul_[RIGHT]) 
    return;

  Scalar prop = get_property ("pletvisibility");
  if (prop.isnum_b ()) 
    plet_spanner_p_->visibility_i_ = prop;

  if ((beam_mom_drul_[LEFT] <= span_mom_drul_[LEFT])
     && (beam_mom_drul_[RIGHT] >= span_mom_drul_[RIGHT]))
     plet_spanner_p_->visibility_i_ &= ~2;

  if (plet_spanner_p_->visibility_i_)
    typeset_element (plet_spanner_p_);
  else
    {
      plet_spanner_p_->unlink ();
      delete plet_spanner_p_;
    }
  
  plet_spanner_p_ = 0;
  span_reqs_drul_[RIGHT] = span_reqs_drul_[LEFT] = 0;
}

