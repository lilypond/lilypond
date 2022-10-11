/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

// FIXME: there is a whole reimplementation of this in Scheme, in
// flag-styles.scm.  It's more flexible, so why do we still have this?
// Shouldn't this be deleted altogether?

#include "stem.hh"

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "international.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "stencil.hh"
#include "warn.hh"

using std::string;

class Flag
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (glyph_name, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_y_offset, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_calc_y_offset, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_x_offset, (SCM));

  static SCM internal_calc_y_offset (SCM smob, bool pure);
};

MAKE_SCHEME_CALLBACK (Flag, width, "ly:flag::width", 1);
SCM
Flag::width (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  auto *sten = unsmob<const Stencil> (get_property (me, "stencil"));
  if (!sten)
    return to_scm (Interval (0.0, 0.0));

  Grob *stem = me->get_x_parent ();

  /*
    TODO:
    This reproduces a bad hard-coding that has been in the code for quite some time:
    the bounding boxes for the flags are slightly off and need to be fixed.
  */

  return to_scm (sten->extent (X_AXIS) - stem->extent (stem, X_AXIS)[RIGHT]);
}

MAKE_SCHEME_CALLBACK (Flag, glyph_name, "ly:flag::glyph-name", 1);
SCM
Flag::glyph_name (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = me->get_x_parent ();

  Direction d = get_grob_direction (stem);
  int log = Stem::duration_log (stem);
  string flag_style;

  SCM flag_style_scm = get_property (me, "style");
  if (scm_is_symbol (flag_style_scm))
    flag_style = ly_symbol2string (flag_style_scm);

  bool adjust = true;

  string staffline_offs;
  if (flag_style == "mensural")
    /* Mensural notation: For notes on staff lines, use different
       flags than for notes between staff lines.  The idea is that
       flags are always vertically aligned with the staff lines,
       regardless if the note head is on a staff line or between two
       staff lines.  In other words, the inner end of a flag always
       touches a staff line.
    */
    {
      if (adjust)
        {
          Real ss = Staff_symbol_referencer::staff_space (me);
          const auto p
            = static_cast<int> (rint (stem->extent (stem, Y_AXIS)[d] * 2 / ss));
          staffline_offs
            = Staff_symbol_referencer::on_line (stem, p) ? "0" : "1";
        }
      else
        staffline_offs = "2";
    }
  else
    staffline_offs = "";

  char dir = (d == UP) ? 'u' : 'd';
  string font_char = flag_style + dir + staffline_offs + std::to_string (log);
  return ly_string2scm ("flags." + font_char);
}

MAKE_SCHEME_CALLBACK (Flag, print, "ly:flag::print", 1);
SCM
Flag::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = me->get_x_parent ();

  Direction d = get_grob_direction (stem);
  string flag_style;

  SCM flag_style_scm = get_property (me, "style");
  if (scm_is_symbol (flag_style_scm))
    flag_style = ly_symbol2string (flag_style_scm);

  if (flag_style == "no-flag")
    return Stencil ().smobbed_copy ();

  char dir = (d == UP) ? 'u' : 'd';
  Font_metric *fm = Font_interface::get_default_font (me);
  string font_char = robust_scm2string (get_property (me, "glyph-name"), "");
  Stencil flag = fm->find_by_name (font_char);
  if (flag.is_empty ())
    me->warning (_f ("flag `%s' not found", font_char));

  /*
    TODO: maybe property stroke-style should take different values,
    e.g. "" (i.e. no stroke), "single" and "double" (currently, it's
    '() or "grace").  */
  SCM stroke_style_scm = get_property (me, "stroke-style");
  if (scm_is_string (stroke_style_scm))
    {
      string stroke_style = ly_scm2string (stroke_style_scm);
      if (!stroke_style.empty ())
        {
          string font_char = flag_style + dir + stroke_style;
          Stencil stroke = fm->find_by_name ("flags." + font_char);
          if (stroke.is_empty ())
            {
              font_char = dir + stroke_style;
              stroke = fm->find_by_name ("flags." + font_char);
            }
          if (stroke.is_empty ())
            me->warning (_f ("flag stroke `%s' not found", font_char));
          else
            flag.add_stencil (stroke);
        }
    }

  return flag.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Flag, pure_calc_y_offset, "ly:flag::pure-calc-y-offset",
                      3);
SCM
Flag::pure_calc_y_offset (SCM smob, SCM /* beg */, SCM /* end */)
{
  return internal_calc_y_offset (smob, true);
}

MAKE_SCHEME_CALLBACK (Flag, calc_y_offset, "ly:flag::calc-y-offset", 1);
SCM
Flag::calc_y_offset (SCM smob)
{
  return internal_calc_y_offset (smob, false);
}

SCM
Flag::internal_calc_y_offset (SCM smob, bool pure)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = me->get_x_parent ();
  Direction d = get_grob_direction (stem);

  Real blot = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  Interval stem_extent = pure ? stem->pure_y_extent (stem, 0, INT_MAX)
                              : stem->extent (stem, Y_AXIS);

  return to_scm (stem_extent.is_empty () ? 0.0 : stem_extent[d] - d * blot / 2);
}

MAKE_SCHEME_CALLBACK (Flag, calc_x_offset, "ly:flag::calc-x-offset", 1);
SCM
Flag::calc_x_offset (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = me->get_x_parent ();
  return to_scm (stem->extent (stem, X_AXIS)[RIGHT]);
}

ADD_INTERFACE (Flag,
               R"(
A flag that gets attached to a stem.The style property is  symbol determining
what style of flag glyph is typeset on a @code{Stem}.  Valid options include
@code{'()} for standard flags, @code{'mensural} and @code{'no-flag}, which
switches off the flag.
               )",

               /* properties */
               R"(
glyph-name
style
stroke-style
               )");
