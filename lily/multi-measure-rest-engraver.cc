/*
  multi_measure_rest-engraver.cc -- implement Multi_measure_rest_engraver

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "multi-measure-rest.hh"
#include "multi-measure-rest-engraver.hh"
#include "score-column.hh"

IMPLEMENT_IS_TYPE_B1 (Multi_measure_rest_engraver, Engraver);
ADD_THIS_TRANSLATOR (Multi_measure_rest_engraver);

Multi_measure_rest_engraver::Multi_measure_rest_engraver ()
{
  last_mom_ = 0;
  multi_measure_req_l_ = 0;
  mmrest_p_ = 0;
}

bool
Multi_measure_rest_engraver::do_try_request (Request* req_l)
{
  if (!req_l->musical () || !req_l->musical ()->multi_measure ())
    return false;

  multi_measure_req_l_ = req_l->musical ()->multi_measure ();
  last_mom_ = now_moment () + multi_measure_req_l_->duration_.length ();

  return true;
}

void
Multi_measure_rest_engraver::do_removal_processing ()
{
  if (mmrest_p_)
    {
      typeset_element (mmrest_p_);
      mmrest_p_ = 0;
    }
}

void
Multi_measure_rest_engraver::do_process_requests ()
{
  if (mmrest_p_ || !multi_measure_req_l_)
    return;

  mmrest_p_ = new Multi_measure_rest;

  Scalar prop = get_property ("part");
  if (prop.isnum_b ()) 
    part_b_ = prop.to_bool ();

//  if (!part_b_)
//    return;

  int measures_i = (int)multi_measure_req_l_->duration_.plet_.iso_i_;
  if (part_b_)
    mmrest_p_->measures_i_ = measures_i;

  announce_element (Score_elem_info (mmrest_p_, multi_measure_req_l_));

  multi_measure_req_l_ = 0;
}

void
Multi_measure_rest_engraver::do_pre_move_processing ()
{
  if (!mmrest_p_)
    return;

  typeset_element (mmrest_p_);
  mmrest_p_ = 0;

  if (part_b_)
    return;

  if (last_mom_ <= now_moment () + Moment (1))
    multi_measure_req_l_ = 0;
}

