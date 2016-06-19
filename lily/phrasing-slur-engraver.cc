/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "slur-engraver.hh"

#include "translator.icc"

class Phrasing_slur_engraver : public Slur_engraver
{
  virtual SCM event_symbol () const;
  virtual bool double_property () const;
  virtual SCM grob_symbol () const;
  virtual const char* object_name () const;
  virtual void set_melisma (bool);

public:
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
  TRANSLATOR_INHERIT (Slur_engraver);
};

SCM
Phrasing_slur_engraver::event_symbol () const
{
  return ly_symbol2scm ("phrasing-slur-event");
}

bool
Phrasing_slur_engraver::double_property () const
{
  return false;
}

SCM
Phrasing_slur_engraver::grob_symbol () const
{
  return ly_symbol2scm ("PhrasingSlur");
}

const char *
Phrasing_slur_engraver::object_name () const
{
  return "phrasing slur";
}

Phrasing_slur_engraver::Phrasing_slur_engraver ()
{
}

void
Phrasing_slur_engraver::set_melisma (bool)
{
}

void
Phrasing_slur_engraver::boot ()
{
  ADD_LISTENER_FOR (Phrasing_slur_engraver, slur, phrasing_slur);
  ADD_LISTENER (Phrasing_slur_engraver, note);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, inline_accidental);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, fingering);
  ADD_ACKNOWLEDGER (Phrasing_slur_engraver, note_column);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, slur);
  ADD_ACKNOWLEDGER (Phrasing_slur_engraver, script);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, dots);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, text_script);
  ADD_END_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, tie);
  ADD_ACKNOWLEDGER_FOR (Phrasing_slur_engraver, extra_object, tuplet_number);
}

ADD_TRANSLATOR (Phrasing_slur_engraver,
                /* doc */
                "Print phrasing slurs.  Similar to @ref{Slur_engraver}.",

                /* create */
                "PhrasingSlur ",

                /* read */
                "",

                /* write */
                ""
               );
