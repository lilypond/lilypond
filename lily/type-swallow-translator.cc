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

#include "translator.hh"
#include "music.hh"

#include "translator.icc"

class Skip_event_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Skip_event_swallow_translator);
};

class Rest_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Rest_swallow_translator);
};

Skip_event_swallow_translator::Skip_event_swallow_translator ()
{
}

ADD_TRANSLATOR (Skip_event_swallow_translator,
		/* doc */
		"Swallow @code{\\skip}.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

Rest_swallow_translator::Rest_swallow_translator (){}

ADD_TRANSLATOR (Rest_swallow_translator,
		/* doc */
		"Swallow rest.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

class Note_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Note_swallow_translator);
};

Note_swallow_translator::Note_swallow_translator ()
{
}

ADD_TRANSLATOR (Note_swallow_translator,
		/* doc */
		"Swallow notes.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

