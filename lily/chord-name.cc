/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "chord-name.hh"

#include "output-def.hh"
#include "font-interface.hh"
#include "paper-column.hh"
#include "system.hh"
#include "staff-symbol-referencer.hh"
#include "text-interface.hh"

MAKE_SCHEME_CALLBACK (Chord_name, after_line_breaking,
                      "ly:chord-name::after-line-breaking", 1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item *me = unsmob<Item> (smob);
  assert (me);

  SCM s = get_property (me, "begin-of-line-visible");
  if (from_scm<bool> (s))
    {
      if (me->get_column ()->get_rank ()
            - me->get_system ()->spanned_column_rank_interval ()[LEFT]
          > 1)
        me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Chord_name,
               R"(
A chord label (name or fretboard).
               )",

               /* properties */
               R"(
begin-of-line-visible
               )");
