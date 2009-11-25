/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>


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

#include "grob.hh"
#include "stream-event.hh"
#include "translator.icc"

class Tweak_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Tweak_engraver);

protected:
  DECLARE_ACKNOWLEDGER (grob);
};

Tweak_engraver::Tweak_engraver ()
{
}

void
Tweak_engraver::acknowledge_grob (Grob_info info)
{
  if (Stream_event *ev = info.event_cause ())
    {
      for (SCM s = ev->get_property ("tweaks");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  info.grob ()->set_property (scm_caar (s), scm_cdar (s));
	}
    }
}

ADD_ACKNOWLEDGER (Tweak_engraver, grob);
ADD_TRANSLATOR (Tweak_engraver,
		/* doc */
		"Read the @code{tweaks} property from the originating event,"
		" and set properties.",
		
		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
