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

#include "note-head.hh"

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "international.hh"
#include "staff-symbol.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

#include <algorithm> //  min, max
#include <cctype>
#include <cmath>
#include <tuple>
#include <utility>

using std::string;

static Stencil
internal_print (Grob *me, string *font_char)
{
  string style = robust_symbol2string (get_property (me, "style"), "default");

  string suffix = std::to_string (
    std::min (from_scm (get_property (me, "duration-log"), 2), 2));
  if (style != "default")
    suffix = robust_scm2string (get_property (me, "glyph-name"), "");

  Font_metric *fm = Font_interface::get_default_font (me);

  string prefix = "noteheads.";
  string idx_symmetric;
  string idx_directed;
  string idx_either = idx_symmetric = prefix + "s";
  Stencil out = fm->find_by_name (idx_either + suffix);
  if (out.is_empty ())
    {
      Grob *stem = unsmob<Grob> (get_object (me, "stem"));
      Direction stem_dir = stem ? get_grob_direction (stem) : CENTER;

      if (stem_dir == CENTER)
        programming_error ("must have stem dir for note head");

      idx_either = idx_directed = prefix + (stem_dir == UP ? "u" : "d");
      out = fm->find_by_name (idx_either + suffix);
    }

  if (style == "mensural" || style == "neomensural" || style == "petrucci"
      || style == "baroque" || style == "kievan")
    {
      if (!Staff_symbol_referencer::on_line (
            me, from_scm (get_property (me, "staff-position"), 0)))
        {
          Stencil test = fm->find_by_name (idx_either + "r" + suffix);
          if (!test.is_empty ())
            {
              idx_either += "r";
              out = test;
            }
        }
    }

  if (style == "kievan" && 3 == from_scm (get_property (me, "duration-log"), 2))
    {
      Grob *stem = unsmob<Grob> (get_object (me, "stem"));
      Grob *beam = unsmob<Grob> (get_object (stem, "beam"));
      if (beam)
        out = fm->find_by_name (idx_either + "2kievan");
    }

  idx_either += suffix;
  if (out.is_empty ())
    {
      me->warning (_f ("none of note heads `%s' or `%s' found",
                       idx_symmetric.c_str (), idx_directed.c_str ()));
      out = Stencil (Box (Interval (0, 0), Interval (0, 0)), SCM_EOL);
    }
  else
    *font_char = idx_either;

  return out;
}

/*
  TODO: make stem X-parent of notehead.
 */
MAKE_SCHEME_CALLBACK (Note_head, stem_x_shift, "ly:note-head::stem-x-shift", 1);
SCM
Note_head::stem_x_shift (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  if (stem)
    (void) get_property (stem, "positioning-done");

  return to_scm (0);
}

MAKE_SCHEME_CALLBACK (Note_head, print, "ly:note-head::print", 1);
SCM
Note_head::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  string idx;
  return internal_print (me, &idx).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Note_head, include_ledger_line_height,
                      "ly:note-head::include-ledger-line-height", 1);
SCM
Note_head::include_ledger_line_height (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);

  if (staff)
    {
      Real ss = Staff_symbol::staff_space (staff);
      Interval lines = Staff_symbol::line_span (staff) * (ss / 2.0);
      Real my_pos = Staff_symbol_referencer::get_position (me) * ss / 2.0;
      Interval my_ext = me->extent (me, Y_AXIS) + my_pos;

      // The +1 and -1 come from the fact that we only want to add
      // the interval between the note and the first ledger line, not
      // the whole interval between the note and the staff.
      Interval iv (std::min (0.0, lines[UP] - my_ext[DOWN] + 1),
                   std::max (0.0, lines[DOWN] - my_ext[UP] - 1));
      return to_scm (iv);
    }

  return to_scm (Interval (0, 0));
}

Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  Offset off = from_scm (get_property (me, "stem-attachment"), Offset (0, 0));

  return off[a];
}

/*
  Stem attachment position for a given stem direction. Each component
  is measured in a -1 to 1 scale, so that -1 is the left/bottom edge of
  the note's bounding box and 1 is the right/top edge.
*/
Offset
Note_head::get_stem_attachment (Font_metric *fm, const string &key,
                                Direction dir)
{
  Offset att;

  size_t k = fm->name_to_index (key);
  if (k != GLYPH_INDEX_INVALID)
    {
      Box b = fm->get_indexed_char_dimensions (k);
      Offset wxwy;
      bool rotate;
      std::tie (wxwy, rotate) = fm->attachment_point (key, dir);
      for (const auto a : {X_AXIS, Y_AXIS})
        {
          Interval v = b[a];
          if (!v.is_empty ())
            {
              att[a] = (2 * (wxwy[a] - v.center ()) / v.length ());
            }
        }
      if (rotate)
        att = -att;
    }

  return att;
}

MAKE_SCHEME_CALLBACK (Note_head, calc_stem_attachment,
                      "ly:note-head::calc-stem-attachment", 1);
SCM
Note_head::calc_stem_attachment (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));
  Font_metric *fm = Font_interface::get_default_font (me);
  string key;
  internal_print (me, &key);

  Direction dir = get_grob_direction (stem);
  if (!dir)
    dir = UP;

  return to_scm (get_stem_attachment (fm, key, dir));
}

/*
  Calculate the default stem attachment for tablature noteheads.
  Hard-coded to (0.0, 1.35) for upward stems and (0.0, -1.35) for
  downward stems.
*/
MAKE_SCHEME_CALLBACK (Note_head, calc_tab_stem_attachment,
                      "ly:note-head::calc-tab-stem-attachment", 1);
SCM
Note_head::calc_tab_stem_attachment (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));

  Direction dir = get_grob_direction (stem);
  if (!dir)
    dir = UP;

  return to_scm (Offset (0.0, dir * 1.35));
}

ADD_INTERFACE (Note_head,
               R"(
A note head.  There are many possible values for @code{style}.  For a complete
list, see @ruser{Note head styles}.
               )",

               /* properties */
               R"(
duration-log
note-names
accidental-grob
ignore-ambitus
glyph-name
stem-attachment
style
ledger-positions
               )");
