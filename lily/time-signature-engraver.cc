/*
  time_signature-reg.cc -- implement Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
Time_signature_engraver::do_process_music()
{
  Translator * result =
    daddy_grav_l()->get_simple_translator ("Timing_engraver");	// ugh

  if (!result)
    {
      warning (_ ("lost in time:"));
      warning (_f ("can't find: `%s'", " Timing_translator"));
      return ;
    }
  
  Timing_engraver * timing_grav_l= dynamic_cast<Timing_engraver *> (result);
  
  Time_signature_change_req *req = timing_grav_l->time_signature_req_l();
  if (req)
    {
      time_signature_p_ = new Time_signature;
      time_signature_p_->set_elt_property ("fraction",
					   gh_cons (gh_int2scm (req->beats_i_),
						    gh_int2scm (req->one_beat_i_))); 
      time_signature_p_->set_elt_property ("break-aligned", SCM_BOOL_T);
    }

  
  if (time_signature_p_)
    announce_element (Score_element_info (time_signature_p_, req));
}

void
Time_signature_engraver::do_pre_move_processing()
{
  if (time_signature_p_) 
    {
      typeset_element (time_signature_p_);
      time_signature_p_ =0;
    }
}


ADD_THIS_TRANSLATOR(Time_signature_engraver);
 

