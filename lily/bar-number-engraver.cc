/*
  bar-number-grav.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "bar-number-engraver.hh"
#include "time-description.hh"
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"
#include "text-item.hh"

Bar_number_engraver::Bar_number_engraver()
{
  axis_ = Y_AXIS;
  type_ = "barNumber";
  visibility_lambda_
    = ly_ch_C_eval_scm ("postbreak_only_visibility");
}

void
Bar_number_engraver::do_process_requests ()
{
  Translator *tr = daddy_grav_l ()->get_simple_translator ("Timing_engraver");
  Timing_translator *timer = dynamic_cast<Timing_translator*>(tr);
  Time_description *time = &timer->time_;

  if (!time->whole_in_measure_ && !time->cadenza_b_ && now_mom () > Moment (0))
    {
      create_items (0);
	
      text_p_->text_str_ = to_str (time->bars_i_);
    }
}

ADD_THIS_TRANSLATOR(Bar_number_engraver);
