/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  void acknowledge_staff_symbol (Grob_info_t<Spanner>);
};

void
Tab_staff_symbol_engraver::acknowledge_staff_symbol (Grob_info_t<Spanner> gi)
{
  long k = scm_ilength (get_property (this, "stringTunings"));
  if (k >= 0)
    set_property (gi.grob (), "line-count", to_scm (k));
}

Tab_staff_symbol_engraver::Tab_staff_symbol_engraver (Context *c)
  : Engraver (c)
{
}

#include "translator.icc"

void
Tab_staff_symbol_engraver::boot ()
{
  ADD_ACKNOWLEDGER (staff_symbol);
}

ADD_TRANSLATOR (Tab_staff_symbol_engraver,
                /* doc */
                R"(
Create a tablature staff symbol, but look at @code{stringTunings} for the
number of lines.
                )",

                /* create */
                R"(
StaffSymbol
                )",

                /* read */
                R"(
stringTunings
                )",

                /* write */
                R"(

                )");
