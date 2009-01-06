/*
  instrument-switch-engraver.cc -- implement

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

*/

#include "engraver.hh"
#include "item.hh"
#include "translator.icc"


class Instrument_switch_engraver : public Engraver
{

  TRANSLATOR_DECLARATIONS (Instrument_switch_engraver);
protected:
  Grob *text_;
  SCM cue_name_;

  void stop_translation_time_step ();
  void process_music ();
};


Instrument_switch_engraver::Instrument_switch_engraver ()
{
  cue_name_ = SCM_EOL;
  text_ = 0;
}

/*
  TODO: should use an event.
 */
void
Instrument_switch_engraver::process_music ()
{
  SCM cue_text = get_property ("instrumentCueName");
  
  if (!scm_is_eq (cue_name_, cue_text))
    {
      text_ = make_item ("InstrumentSwitch", SCM_EOL);
      text_->set_property ("text", cue_text);
      cue_name_ = cue_text;
    }
}

void
Instrument_switch_engraver::stop_translation_time_step ()
{
  text_ = 0;
}

ADD_TRANSLATOR (Instrument_switch_engraver,
		/* doc */
		"Create a cue text for taking instrument.",
			
		/* create */
		"InstrumentSwitch ",

		/* read */
		"instrumentCueName ",
			
		/* write */
		""
		);
