/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"

#include "dots.hh"
#include "duration.hh"
#include "item.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"

#include "translator.icc"

class Rest_engraver : public Engraver
{
  Stream_event *rest_event_;
  Item *dot_;
  Grob *rest_;
protected:
  void start_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (rest);
public:
  TRANSLATOR_DECLARATIONS (Rest_engraver);
};

/*
  Should merge with Note_head_engraver
*/
Rest_engraver::Rest_engraver ()
{
  rest_event_ = 0;
  rest_ = 0;
  dot_ = 0;
}

void
Rest_engraver::start_translation_timestep ()
{
  rest_event_ = 0;
  rest_ = 0;
  dot_ = 0;
}

void
Rest_engraver::process_music ()
{
  if (rest_event_ && !rest_)
    {
      rest_ = make_item ("Rest", rest_event_->self_scm ());
      Pitch *p = unsmob_pitch (rest_event_->get_property ("pitch"));

      if (p)
	{
	  int pos = p->steps ();
	  SCM c0 = get_property ("middleCPosition");
	  if (scm_is_number (c0))
	    pos += scm_to_int (c0);

	  rest_->set_property ("staff-position", scm_from_int (pos));
	}
    }
}

IMPLEMENT_TRANSLATOR_LISTENER (Rest_engraver, rest);
void
Rest_engraver::listen_rest (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (rest_event_, ev);
}

ADD_TRANSLATOR (Rest_engraver,
		/* doc */
		"Engrave rests.",

		/* create */
		"Rest ",

		/* read */
		"middleCPosition ",

		/* write */
		""
		);
