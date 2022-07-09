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

#include "slur-engraver.hh"

#include "item.hh"

#include "translator.icc"

class Phrasing_slur_engraver : public Slur_engraver
{
  SCM event_symbol () const override;
  bool double_property () const override;
  SCM grob_symbol () const override;
  const char *object_name () const override;
  void set_melisma (bool) override;

public:
  TRANSLATOR_DECLARATIONS (Phrasing_slur_engraver);
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

Phrasing_slur_engraver::Phrasing_slur_engraver (Context *c)
  : Slur_engraver (c)
{
}

void
Phrasing_slur_engraver::set_melisma (bool)
{
}

void
Phrasing_slur_engraver::boot ()
{
  ADD_LISTENER_FOR (listen_slur, phrasing_slur);
  ADD_LISTENER (note);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, inline_accidental);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, fingering);
  ADD_ACKNOWLEDGER (note_column);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, slur);
  ADD_ACKNOWLEDGER (script);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, dots);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, text_script);
  ADD_END_ACKNOWLEDGER_FOR (acknowledge_extra_object, tie);
  ADD_ACKNOWLEDGER_FOR (acknowledge_extra_object, tuplet_number);
}

ADD_TRANSLATOR (Phrasing_slur_engraver,
                /* doc */
                R"(
Print phrasing slurs.  Similar to @ref{Slur_engraver}.
                )",

                /* create */
                R"(
PhrasingSlur
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
