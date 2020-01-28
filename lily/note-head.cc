/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <algorithm> //  min, max
#include <cctype>
#include <cmath>

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "international.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "warn.hh"

using std::string;

static Stencil
internal_print (Grob *me, string *font_char)
{
  string style = robust_symbol2string (me->get_property ("style"), "default");

  string suffix = std::to_string (
      std::min (robust_scm2int (me->get_property ("duration-log"), 2), 2));
  if (style != "default")
    suffix = robust_scm2string (me->get_property ("glyph-name"), "");

  Font_metric *fm = Font_interface::get_default_font (me);

  string prefix = "noteheads.";
  string idx_symmetric;
  string idx_directed;
  string idx_either = idx_symmetric = prefix + "s";
  Stencil out = fm->find_by_name (idx_either + suffix);
  if (out.is_empty ())
    {
      Grob *stem = unsmob<Grob> (me->get_object ("stem"));
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
              me, robust_scm2int (me->get_property ("staff-position"), 0)))
        {
          Stencil test = fm->find_by_name (idx_either + "r" + suffix);
          if (!test.is_empty ())
            {
              idx_either += "r";
              out = test;
            }
        }
    }

  if (style == "kievan"
      && 3 == robust_scm2int (me->get_property ("duration-log"), 2))
    {
      Grob *stem = unsmob<Grob> (me->get_object ("stem"));
      Grob *beam = unsmob<Grob> (stem->get_object ("beam"));
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
MAKE_SCHEME_CALLBACK (Note_head, stem_x_shift, 1);
SCM
Note_head::stem_x_shift (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Grob *stem = unsmob<Grob> (me->get_object ("stem"));
  if (stem)
    (void)stem->get_property ("positioning-done");

  return scm_from_int (0);
}

MAKE_SCHEME_CALLBACK (Note_head, print, 1);
SCM
Note_head::print (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);

  string idx;
  return internal_print (me, &idx).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Note_head, include_ledger_line_height, 1);
SCM
Note_head::include_ledger_line_height (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
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
      return ly_interval2scm (iv);
    }

  return ly_interval2scm (Interval (0, 0));
}

Real
Note_head::stem_attachment_coordinate (Grob *me, Axis a)
{
  Offset off
      = robust_scm2offset (me->get_property ("stem-attachment"), Offset (0, 0));

  return off[a];
}

Offset
Note_head::get_stem_attachment (Font_metric *fm, const string &key)
{
  Offset att;

  size_t k = fm->name_to_index (key);
  if (k != GLYPH_INDEX_INVALID)
    {
      Box b = fm->get_indexed_char_dimensions (k);
      Offset wxwy = fm->attachment_point (key);
      for (int i = X_AXIS; i < NO_AXES; i++)
        {
          Axis a = Axis (i);

          Interval v = b[a];
          if (!v.is_empty ())
            {
              att[a] = (2 * (wxwy[a] - v.center ()) / v.length ());
            }
        }
    }

  return att;
}

MAKE_SCHEME_CALLBACK (Note_head, calc_stem_attachment, 1);
SCM
Note_head::calc_stem_attachment (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  Font_metric *fm = Font_interface::get_default_font (me);
  string key;
  internal_print (me, &key);

  return ly_offset2scm (get_stem_attachment (fm, key));
}

ADD_INTERFACE (Note_head,
               "A note head.  There are many possible values for"
               " @code{style}.  For a complete list, see"
               " @ruser{Note head styles}.",

               /* properties */
               "duration-log "
               "note-names "
               "accidental-grob "
               "ignore-ambitus "
               "glyph-name "
               "stem-attachment "
               "style "
               "ledger-positions ");
