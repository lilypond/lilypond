/*
  meter-reg.cc -- implement Meter_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "meter-grav.hh"
#include "meter.hh"
#include "command-request.hh"
#include "timing-grav.hh"
#include "engraver-group.hh"

Meter_engraver::Meter_engraver()
{ 
  meter_p_ =0;
}

void
Meter_engraver::do_process_requests()
{
  Translator * result =
    daddy_grav_l()->get_simple_translator (Timing_engraver::static_name());

  if (!result)
    {
      warning ("Lost in time: can't find Timing_translator");
      return ;
    }
  
  Timing_engraver * timing_grav_l= (Timing_engraver*) result->engraver_l ();
  
  Meter_change_req *req = timing_grav_l->meter_req_l();
  if (req)
    {
      Array<Scalar> args;
      args.push (req->beats_i_);
      args.push (req->one_beat_i_);
	
      meter_p_ = new Meter (args);
      meter_p_->break_priority_i_ = 1; // ugh
    }

  if (meter_p_)
    announce_element (Score_elem_info (meter_p_, req));
}

void
Meter_engraver::do_pre_move_processing()
{
  if (meter_p_) 
    {
      typeset_element (meter_p_);
      meter_p_ =0;
    }
}


ADD_THIS_TRANSLATOR(Meter_engraver);
IMPLEMENT_IS_TYPE_B1(Meter_engraver,Engraver); 
