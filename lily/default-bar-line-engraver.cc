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

#include "context.hh"
#include "grob.hh"

class Default_bar_line_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Default_bar_line_engraver);
};

#include "translator.icc"

void
Default_bar_line_engraver::boot ()
{

}

ADD_TRANSLATOR (Default_bar_line_engraver,
                /* doc */
                R"(
This engraver does nothing and should be removed.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
                )",

                /* write */
                R"(
                )");

Default_bar_line_engraver::Default_bar_line_engraver (Context *c)
  : Engraver (c)
{
}
