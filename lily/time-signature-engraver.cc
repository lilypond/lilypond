/*
  time_signature-reg.cc -- implement Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "time-signature-engraver.hh"
#include "time-signature.hh"
#include "command-request.hh"
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"

Time_signature_engraver::Time_signature_engraver()
{ 
  time_signature_p_ =0;
}

void
Time_signature_engraver::do_process_requests()
{
  Translator * result =
    daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh

  if (!result)
    {
      warning (_ ("lost in time:"));
      warning (_f ("Can't find: `%s'", " Timing_translator"));
      return ;
    }
  
  Timing_engraver * timing_grav_l= dynamic_cast<Timing_engraver *> (result);
  
  Time_signature_change_req *req = timing_grav_l->time_signature_req_l();
  if (req)
    {
      Array<int> args;
      args.push (req->beats_i_);
      args.push (req->one_beat_i_);
	
      time_signature_p_ = new Time_signature ();
      time_signature_p_->args_ = args;
      time_signature_p_->set_elt_property (break_priority_scm_sym, gh_int2scm (1)); // 1
    }

  
  if (time_signature_p_)
    announce_element (Score_element_info (time_signature_p_, req));
}

void
Time_signature_engraver::do_pre_move_processing()
{
  if (time_signature_p_) 
    {
      Scalar sigstyle = get_property ("timeSignatureStyle", 0);
      if (sigstyle.length_i ())
	{
	  time_signature_p_->time_sig_type_str_ = sigstyle;
	}

      typeset_element (time_signature_p_);
      time_signature_p_ =0;
    }
}


ADD_THIS_TRANSLATOR(Time_signature_engraver);
 
