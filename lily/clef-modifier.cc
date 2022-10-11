/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2014--2022 Janek Warchoł <lemniskata.bernoullego@gmail.com>

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

#include "item.hh"

using std::string;

struct Clef_modifier
{
  DECLARE_SCHEME_CALLBACK (calc_parent_alignment, (SCM));
};

MAKE_SCHEME_CALLBACK (Clef_modifier, calc_parent_alignment,
                      "ly:clef-modifier::calc-parent-alignment", 1)
SCM
Clef_modifier::calc_parent_alignment (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *clef = me->get_x_parent ();
  string full_clef_name = ly_scm2string (get_property (clef, "glyph"));
  string clef_name = replace_all (&full_clef_name, "clefs.", "");

  // find entry with keyname clef_type in clef-alignments
  SCM alist_entry = scm_assq (ly_symbol2scm (clef_name),
                              get_property (me, "clef-alignments"));

  if (scm_is_pair (alist_entry))
    {
      SCM entry_value = scm_cdr (alist_entry);
      // the value should be a pair of numbers - first is the alignment
      // for modifiers below the clef, second for those above.
      if (scm_is_pair (entry_value))
        if (from_scm (get_property (me, "direction"), DOWN) == DOWN)
          return scm_car (entry_value);
        else
          return scm_cdr (entry_value);

      else // default alignment = centered
        return to_scm (0);
    }
  else // default alignment = centered
    return to_scm (0);
}

ADD_INTERFACE (Clef_modifier,
               R"(
The number describing transposition of the clef, placed below or above clef
sign.  Usually this is 8 (octave transposition) or 15 (two octaves), but
LilyPond allows any integer here.
               )",

               /* properties */
               R"(
clef-alignments
               )");
