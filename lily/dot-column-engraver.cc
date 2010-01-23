/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "translator.icc"
#include "item.hh"

class Dot_column_engraver : public Engraver
{
  Grob *dotcol_;
public:
  TRANSLATOR_DECLARATIONS (Dot_column_engraver);

protected:

  DECLARE_ACKNOWLEDGER (rhythmic_head);

  void stop_translation_timestep ();
};

Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_ = 0;
}

void
Dot_column_engraver::stop_translation_timestep ()
{
  dotcol_ = 0;
}

void
Dot_column_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  Grob *d = unsmob_grob (info.grob ()->get_object ("dot"));
  if (d)
    {
      if (!dotcol_)
	dotcol_ = make_item ("DotColumn", SCM_EOL);

      Dot_column::add_head (dotcol_, info.grob ());
    }
}


ADD_ACKNOWLEDGER (Dot_column_engraver, rhythmic_head);
ADD_TRANSLATOR (Dot_column_engraver,
		/* doc */
		"Engrave dots on dotted notes shifted to the right of the"
		" note.  If omitted, then dots appear on top of the notes.",

		/* create */
		"DotColumn ",

		/* read */
		"",

		/* write */
		""
		);
