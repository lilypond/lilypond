/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "staff-symbol.hh"
#include "engraver.hh"
#include "context.hh"
#include "spanner.hh"

class Staff_collecting_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Staff_collecting_engraver);
  void acknowledge_staff_symbol (Grob_info_t<Spanner>);
  void acknowledge_end_staff_symbol (Grob_info_t<Spanner>);
};

Staff_collecting_engraver::Staff_collecting_engraver (Context *c)
  : Engraver (c)
{
}

void
Staff_collecting_engraver::acknowledge_staff_symbol (Grob_info_t<Spanner> gi)
{
  SCM staffs = get_property (this, "stavesFound");
  staffs = scm_cons (gi.grob ()->self_scm (), staffs);

  set_property (context (), "stavesFound", staffs);
}

void
Staff_collecting_engraver::acknowledge_end_staff_symbol (
  Grob_info_t<Spanner> gi)
{
  SCM staffs = get_property (this, "stavesFound");
  staffs = scm_delq (gi.grob ()->self_scm (), staffs);

  set_property (context (), "stavesFound", staffs);
}

#include "translator.icc"

void
Staff_collecting_engraver::boot ()
{
  ADD_ACKNOWLEDGER (staff_symbol);
  ADD_END_ACKNOWLEDGER (staff_symbol);
}

ADD_TRANSLATOR (Staff_collecting_engraver,
                /* doc */
                R"(
Maintain the @code{stavesFound} variable.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
stavesFound
                )",

                /* write */
                R"(
stavesFound
                )");
