/*
  bar-number-grav.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "bar-number-engraver.hh"
#include "timing-translator.hh"
#include "timing-engraver.hh"
#include "engraver-group-engraver.hh"
#include "text-item.hh"

Bar_number_engraver::Bar_number_engraver()
{
  axis_ = Y_AXIS;
  type_ = "barNumber";
  visibility_lambda_
    = ly_ch_C_eval_scm ("postbreak-only-visibility");
}

void
Bar_number_engraver::do_process_requests ()
{
  Translator *tr = daddy_grav_l ()->get_simple_translator ("Timing_engraver");
  Timing_translator *time = dynamic_cast<Timing_translator*>(tr);

  // todo include (&&!time->cadenza_b_ )
  if (!time->measure_position () && now_mom () > Moment (0))
    {
      create_items (0);
	
      text_p_->text_str_ = to_str (time->bars_i ());
    }
}

ADD_THIS_TRANSLATOR(Bar_number_engraver);
