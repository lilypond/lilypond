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

//  Junk String numbers.
class String_number_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (String_number_engraver);
protected:
  virtual bool try_music (Music *m);
};

bool
String_number_engraver::try_music (Music *)
{
  return true;
}

String_number_engraver::String_number_engraver ()
{
}

/*
  TODO: string numbers are printed right of the note circled. This
  engraver should provide this functionality.
*/

#include "translator.icc"

ADD_TRANSLATOR (String_number_engraver,
		/* doc */
		"Swallow string number events.  The purpose of this engraver"
		" is to process tablatures for normal notation.  To provent"
		" warnings for unprocessed string number events to obscure"
		" real error messages, this engraver swallows them all.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
