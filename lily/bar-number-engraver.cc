/*
  bar-number-grav.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "bar-number-engraver.hh"
#include "time-description.hh"
#include "timing-engraver.hh"
#include "engraver-group.hh"
#include "g-text-item.hh"

Bar_number_engraver::Bar_number_engraver()
{
  axis_ = Y_AXIS;
  type_ = "barNumber";
  visibility_lambda_
    = gh_eval_str ("(lambda (d) (if (= d 1) '(#f . #f) '(#t . #t)))");
}

void
Bar_number_engraver::do_process_requests ()
{
  Translator *tr = daddy_grav_l ()->get_simple_translator ("Timing_engraver");
  Timing_translator *timer = dynamic_cast<Timing_translator*>(tr);
  Time_description *time = &timer->time_;

  if (!time->whole_in_measure_ && !time->cadenza_b_ && now_moment () > Moment (0))
    {
      create_items (0);
	
      text_p_->text_str_ = to_str (time->bars_i_);
      text_p_->style_str_ = "roman";
    }
}

ADD_THIS_TRANSLATOR(Bar_number_engraver);
