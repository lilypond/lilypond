/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "horizontal-bracket.hh"

#include "bracket.hh"
#include "stencil.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "spanner.hh"
#include "item.hh"

using std::vector;

MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, "ly:horizontal-bracket::print",
                      1);
SCM
Horizontal_bracket::print (SCM smob)
{
  Spanner *me = unsmob<Spanner> (smob);
  extract_grob_set (me, "columns", gs);

  vector<Grob *> enclosed = gs;
  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  for (const auto d : {LEFT, RIGHT})
    {
      Item *b = me->get_bound (d);
      if (b->break_status_dir ())
        enclosed.push_back (b);
    }

  Stencil b = Bracket::make_enclosing_bracket (me, me, enclosed, X_AXIS,
                                               get_grob_direction (me));
  return b.smobbed_copy ();
}

ADD_INTERFACE (Horizontal_bracket,
               R"(
A horizontal bracket encompassing notes.
               )",

               /* properties */
               R"(
bracket-flare
bracket-text
columns
dashed-edge
edge-height
shorten-pair
connect-to-neighbor
               )");
