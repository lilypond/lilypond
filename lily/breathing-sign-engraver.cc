/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Michael Krause

  written for the GNU LilyPond music typesetter

  TODO:

  . Cancel any beams running through the breathing sign
  ([e8 \breathe f e f] should become [e8] \breathe [f e f])
  . Spacing is not yet completely pretty

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
