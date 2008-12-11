/*
  breathing_sign-engraver.cc -- implement Breathing_sign_engraver

  (c) 1999--2008 Michael Krause

  written for the GNU LilyPond music typesetter

  TODO:

  . Cancel any beams running through the breathing sign
  ([e8 \breathe f e f] should become [e8] \breathe [f e f])
  . Spacing is not yet completely pretty
*/

#include "breathing-sign.hh"
#include "engraver.hh"
#include "item.hh"
#include "stream-event.hh"

#include "translator.icc"

class Breathing_sign_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Breathing_sign_engraver);

protected:
  void process_music ();
  void stop_translation_timestep ();

  DECLARE_TRANSLATOR_LISTENER (breathing);
private:
  Stream_event *breathing_sign_event_;
  Grob *breathing_sign_;
};

Breathing_sign_engraver::Breathing_sign_engraver ()
{
  breathing_sign_ = 0;
  breathing_sign_event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Breathing_sign_engraver, breathing);
void
Breathing_sign_engraver::listen_breathing (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (breathing_sign_event_, ev);
}

void
Breathing_sign_engraver::process_music ()
{
  if (breathing_sign_event_)
    {
      breathing_sign_ = make_item ("BreathingSign", breathing_sign_event_->self_scm ());
    }
}

void
Breathing_sign_engraver::stop_translation_timestep ()
{
  breathing_sign_ = 0;
  breathing_sign_event_ = 0;
}

ADD_TRANSLATOR (Breathing_sign_engraver,
		/* doc */
		"Create a breathing sign.",

		/* create */
		"BreathingSign ",

		/* read */
		"",

		/* write */
		""
		);
