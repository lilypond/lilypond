/*
  time_signature-reg.cc -- implement Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "time-signature-engraver.hh"
#include "time-signature.hh"
#include "command-request.hh"
#include "timing-engraver.hh"
#include "engraver-group.hh"

Time_signature_engraver::Time_signature_engraver()
{ 
  time_signature_p_ =0;
}

void
Time_signature_engraver::do_process_requests()
{
  Translator * result =
    daddy_grav_l()->get_simple_translator (Timing_engraver::static_name());

  if (!result)
    {
      warning (_ ("lost in time") + ": " + _ ("can't find") 
        + " Timing_translator");
      return ;
    }
  
  Timing_engraver * timing_grav_l= (Timing_engraver*) result->access_Engraver  ();
  
  Time_signature_change_req *req = timing_grav_l->time_signature_req_l();
  if (req)
    {
      Array<Scalar> args;
      args.push (req->beats_i_);
      args.push (req->one_beat_i_);
	
      time_signature_p_ = new Time_signature ();
      time_signature_p_->args_ = args;
      time_signature_p_->break_priority_i_ = 1; // ugh
    }

  
  if (time_signature_p_)
    announce_element (Score_element_info (time_signature_p_, req));
}

void
Time_signature_engraver::do_pre_move_processing()
{
  if (time_signature_p_) 
    {
      Scalar sigstyle = get_property ("timeSignatureStyle");
      if (sigstyle.length_i ())
	{
	  time_signature_p_->time_sig_type_str_ = sigstyle;
	}

      typeset_element (time_signature_p_);
      time_signature_p_ =0;
    }
}


ADD_THIS_TRANSLATOR(Time_signature_engraver);
IMPLEMENT_IS_TYPE_B1(Time_signature_engraver,Engraver); 
