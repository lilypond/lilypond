/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "rhythmic-head.hh"
#include "grob.hh"

class Pitch_squash_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Pitch_squash_engraver);
  void acknowledge_note_head (Grob_info);
};

void
Pitch_squash_engraver::acknowledge_note_head (Grob_info i)
{
  SCM newpos = get_property (this, "squashedPosition");
  if (scm_is_number (newpos))
    set_property (i.grob (), "staff-position", newpos);
}

Pitch_squash_engraver::Pitch_squash_engraver (Context *c)
  : Engraver (c)
{
}

#include "translator.icc"
void
Pitch_squash_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Pitch_squash_engraver,
                /* doc */
                R"(
Set the vertical position of note heads to @code{squashedPosition}, if that
property is set.  This can be used to make a single-line staff demonstrating
the rhythm of a melody.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
squashedPosition
                )",

                /* write */
                R"(

                )");
