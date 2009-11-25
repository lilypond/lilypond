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
#include "spanner.hh"

class Tab_staff_symbol_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Tab_staff_symbol_engraver);
protected:
  DECLARE_ACKNOWLEDGER (staff_symbol);
};

void
Tab_staff_symbol_engraver::acknowledge_staff_symbol (Grob_info gi)
{
  int k = scm_ilength (get_property ("stringTunings"));
  if (k >= 0)
    gi.grob ()->set_property ("line-count", scm_from_int (k));
}

Tab_staff_symbol_engraver::Tab_staff_symbol_engraver ()
{
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Tab_staff_symbol_engraver, staff_symbol);
ADD_TRANSLATOR (Tab_staff_symbol_engraver,
		/* doc */
		"Create a tablature staff symbol, but look at"
		" @code{stringTunings} for the number of lines.",

		/* create */
		"StaffSymbol ",

		/* read */
		"stringTunings ",

		/* write */
		""
		);
