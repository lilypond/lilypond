/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "bar-line.hh"

#include "all-font-metrics.hh"
#include "font-interface.hh"
#include "line-interface.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Bar_line, calc_bar_extent, 1)
SCM
Bar_line::calc_bar_extent (SCM smob)
{
  Interval result;
  Grob *me = unsmob_grob (smob);
  if (Grob *staff = Staff_symbol_referencer::get_staff_symbol (me))
    result = staff->extent (staff, Y_AXIS);

  return ly_interval2scm (result);
}

Interval
Bar_line::bar_y_extent (Grob *me, Grob *refpoint)
{
  Interval iv = robust_scm2interval (me->get_property ("bar-extent"), Interval ());

  iv.translate (me->relative_coordinate (refpoint, Y_AXIS));
  return iv;
}

bool
Bar_line::non_empty_barline (Grob *me)
{
  return has_interface (me) && !me->extent (me, X_AXIS).is_empty ();
}

MAKE_SCHEME_CALLBACK (Bar_line, print, 1);
SCM
Bar_line::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM s = me->get_property ("glyph-name");
  SCM extent = me->get_property ("bar-extent");

  if (scm_is_string (s) && is_number_pair (extent))
    {
      string str = ly_scm2string (s);
      Interval ex = ly_scm2interval (extent);
      if (ex.length () > 0)
        {
          Stencil result = compound_barline (me, str, ex, false);

          return result.smobbed_copy ();
        }
    }
  return SCM_EOL;
}

Stencil
Bar_line::compound_barline (Grob *me, string str, Interval const &extent,
                            bool rounded)
{
  Real kern = robust_scm2double (me->get_property ("kern"), 1);
  Real thinkern = robust_scm2double (me->get_property ("thin-kern"), 1);
  Real hair = robust_scm2double (me->get_property ("hair-thickness"), 1);
  Real fatline = robust_scm2double (me->get_property ("thick-thickness"), 1);

  Real staffline = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  kern *= staffline;
  thinkern *= staffline;
  hair *= staffline;
  fatline *= staffline;

  Stencil thin = simple_barline (me, hair, extent, rounded);
  Stencil thick = simple_barline (me, fatline, extent, rounded);
  Stencil dot = Font_interface::get_default_font (me)->find_by_name ("dots.dot");

  int lines = Staff_symbol_referencer::line_count (me);
  Real dist
    = ((lines & 1 || lines == 0)
       ? 1
       : (staff_space < 2 ? 2 : .5)) * staff_space;
  Stencil colon (dot);
  colon.translate_axis (dist, Y_AXIS);
  colon.add_stencil (dot);
  colon.translate_axis (-dist / 2, Y_AXIS);

  Real const h = extent.length ();
  Stencil m;

  if (str == "||:")
    str = "|:";

  if (str == "|S" || str == "S|")
    str = "S";

  if (str == "")
    {
      Stencil empty = Lookup::blank (Box (Interval (0, 0), extent));
      return empty;
    }
  else if (str == "|")
    return thin;
  else if (str == ".")
    return thick;
  else if (str == "|." || (h == 0 && str == ":|"))
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
    }
  else if (str == ".|" || (h == 0 && str == "|:"))
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
    }
  else if (str == ":|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);
    }
  else if (str == "|:")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);
    }
  else if (str == ":|.|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);
    }
  else if (str == ":|.:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);
    }
  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);
    }
  else if (str == "|.|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
    }
  else if (str == "||")
    {
      /*
        should align to other side? this never appears
        on the system-start?
        m.add_at_edge (X_AXIS, RIGHT, thin, 0);
        m.add_at_edge (X_AXIS, RIGHT, thin, thinkern);
      */
      m.add_at_edge (X_AXIS, LEFT, thin, thinkern);
      m.add_at_edge (X_AXIS, RIGHT, thin, thinkern);
    }
  else if (str.find ("S") != NPOS || str == "|._.|")
    {
      //  Handle all varsegno stuff
      Stencil segno;
      segno.add_at_edge (X_AXIS, LEFT, thin, thinkern);
      segno.add_at_edge (X_AXIS, RIGHT, thin, thinkern);
      segno.add_stencil (Font_interface::get_default_font (me)->find_by_name ("scripts.varsegno"));

      if (str == "S")
        m.add_stencil (segno);
      else if (str == "S|:" || str == ".S|:")
        {
          m.add_at_edge (X_AXIS, RIGHT, thick, 0);
          m.add_at_edge (X_AXIS, RIGHT, thin, kern);
          m.add_at_edge (X_AXIS, RIGHT, colon, kern);
          m.add_at_edge (X_AXIS, LEFT, segno, thinkern);
        }
      else if (str == ":|S" || str == ":|S.")
        {
          m.add_at_edge (X_AXIS, LEFT, thick, 0);
          m.add_at_edge (X_AXIS, LEFT, thin, kern);
          m.add_at_edge (X_AXIS, LEFT, colon, kern);
          m.add_at_edge (X_AXIS, RIGHT, segno, thinkern);
        }
      else if (str == ":|S|:" || str == ":|S.|:")
        {
          m.add_at_edge (X_AXIS, LEFT, thick, 0);
          m.add_at_edge (X_AXIS, LEFT, thin, kern);
          m.add_at_edge (X_AXIS, LEFT, colon, kern);
          m.add_at_edge (X_AXIS, RIGHT, segno, thinkern);
          m.add_at_edge (X_AXIS, RIGHT, thick, thinkern);
          m.add_at_edge (X_AXIS, RIGHT, thin, kern);
          m.add_at_edge (X_AXIS, RIGHT, colon, kern);
        }
      else if (str == "|._.|") // :|S|: or :|S.|: without segno and colon
        {
          // get the width of the segno sign
          Real segno_width = segno.extent (X_AXIS).length ();
          m.add_at_edge (X_AXIS, LEFT, thick, 0);
          m.add_at_edge (X_AXIS, LEFT, thin, kern);
          m.add_at_edge (X_AXIS, RIGHT, thick, segno_width + 2 * thinkern);
          m.add_at_edge (X_AXIS, RIGHT, thin, kern);
        }
      // end varsegno block
    }
  else if (str == ":")
    {
      if (Grob *staff = Staff_symbol_referencer::get_staff_symbol (me))
        {
          Interval staff_extent = staff->extent (staff, Y_AXIS);

          /*
            assume staff lines are disposed equally at unit space;
            put a dot into each space within extent (may extend staff_extent).

            staff_extent is an interval of two integers or two half-integers;
            in the former case dots are to be placed at half-integers,
            in the latter at integers.

            these integers are not exact due to staff line thickness.
          */
          int const pos = int (rint (staff_extent.at (UP) * 2));
          Real const correction = pos & 1 ? 0.0 : 0.5;

          for (int i = int (rint (extent.at (DOWN) + (0.5 - correction))),
               e = int (rint (extent.at (UP) + (0.5 - correction)));
               i < e;
               ++i)
            {
              Stencil d (dot);

              d.translate_axis (i + correction, Y_AXIS);
              m.add_stencil (d);
            }
        }
    }
  else if (str == "dashed")
    m = dashed_bar_line (me, extent, hair);
  else if (str == "'")
    m = tick_bar_line (me, extent.at (UP), rounded);

  return m;
}

Stencil
Bar_line::simple_barline (Grob *me,
                          Real w,
                          Interval const &extent,
                          bool rounded)
{
  Real blot
    = rounded
      ? me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"))
      : 0.0;

  return Lookup::round_filled_box (Box (Interval (0, w), extent), blot);
}

Stencil
Bar_line::tick_bar_line (Grob *me, Real h, bool rounded)
{
  Real th = Staff_symbol_referencer::staff_space (me) / 2;
  Real line_thick = Staff_symbol_referencer::line_thickness (me);

  Real blot
    = rounded
      ? me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"))
      : 0.0;

  return Lookup::round_filled_box (Box (Interval (0, line_thick),
                                        Interval (h - th, h + th)), blot);
}

Stencil
Bar_line::dashed_bar_line (Grob *me, Interval const &extent, Real thick)
{
  Real dash_size
    = 1.0 - robust_scm2double (me->get_property ("gap"), 0.3);
  /*
    this is a tad complex for what we want to achieve, but with a
    simple line, the round blotting interferes with staff line
    connections.
  */
  Real ss = Staff_symbol_referencer::staff_space (me);
  Real const h = extent.length ();
  int dashes = int (rint (h / ss));

  /*
    there are two concerns:
    1. one dash plus one space should be one staff space
    2. the line should begin and end with half a dash

    both can be satisfied, if the extent is (roughly) an integer
    multiple of staff space.
  */
  if (fabs (h / ss - dashes) < 0.1)
    {
      Real blot
        = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

      Real const half_dash = dash_size / 2;
      Stencil bar;

      for (int i = 0; i <= dashes; ++i)
        {
          Real top_y = extent.at (DOWN)
                       + (i == dashes ? h : (i + half_dash) * ss);
          Real bot_y = extent.at (DOWN) + (i ? (i - half_dash) * ss : 0.0);

          bar.add_stencil (Lookup::round_filled_box (Box (Interval (0, thick),
                                                          Interval (bot_y, top_y)),
                                                     blot));
        }
      return bar;
    }
  else
    {
      /*
        We have to scale the dashing so it starts and ends with half a
        dash exactly.
      */
      Real total_dash_size = h / dashes;
      Real factor = (dash_size - thick) / ss;

      SCM at = scm_list_n (ly_symbol2scm ("dashed-line"),
                           scm_from_double (thick),
                           scm_from_double (factor * total_dash_size),
                           scm_from_double ((1 - factor) * total_dash_size),
                           scm_from_double (0),
                           scm_from_double (h),
                           scm_from_double (factor * total_dash_size * 0.5),
                           SCM_UNDEFINED);

      Box box;
      box.add_point (Offset (0, 0));
      box.add_point (Offset (0, h));

      Stencil s (box, at);
      s.translate (Offset (thick / 2, extent.at (DOWN)));
      return s;
    }
  return Stencil ();
}

MAKE_SCHEME_CALLBACK (Bar_line, calc_anchor, 1)
SCM
Bar_line::calc_anchor (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Real kern = robust_scm2double (me->get_property ("kern"), 1);
  Real staffline = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));
  string str = robust_scm2string (me->get_property ("glyph-name"), "");

  /* we put the anchor in the center of the barline, unless we are
     a repeat bar, in which case we put the anchor in the center of
     the barline without the dots. */
  Interval ext = me->extent (me, X_AXIS);
  if (ext.is_empty ())
    return scm_from_double (0);

  Real anchor = ext.center ();

  Stencil dot = Font_interface::get_default_font (me)->find_by_name ("dots.dot");
  Real dot_width = dot.extent (X_AXIS).length () + kern * staffline;
  if (str == "|:")
    anchor -= dot_width / 2;
  else if (str == ":|")
    anchor += dot_width / 2;

  return scm_from_double (anchor);
}

ADD_INTERFACE (Bar_line,
               "Bar line.\n"
               "\n"
               "Print a special bar symbol.  It replaces the regular bar"
               " symbol with a special symbol.  The argument @var{bartype}"
               " is a string which specifies the kind of bar line to print."
               "  Options are @code{|}, @code{:|}, @code{|:}, @code{:|:}, @code{:|.|:},"
               " @code{:|.:}, @code{.}, @code{||}, @code{|.}, @code{.|}, @code{.|.},"
               " @code{|.|}, @code{:}, @code{dashed}, @code{'} and @code{S}.\n"
               "\n"
               "These produce, respectively, a normal bar line, a right repeat, a left repeat,"
               " a thick double repeat, a thin-thick-thin double repeat,"
               " a thin-thick double repeat, a thick bar, a double bar, a start bar,"
               " an end bar, a thick double bar, a thin-thick-thin bar,"
               " a dotted bar, a dashed bar, a tick as bar line and a segno bar.\n"
               "\n"
               "In addition, there is an option"
               " @code{||:} which is equivalent to @code{|:} except at line"
               " breaks, where it produces a double bar (@code{||}) at the"
               " end of the line and a repeat sign (@code{|:}) at the"
               " beginning of the new line.\n"
               "\n"
               "For segno, @code{S} produces a segno sign except at line breaks,"
               " where it produces a double bar (@code{||}) at the"
               " end of the line and a segno sign at the beginning of the new line."
               " @code{|S} is equivalent to @code{S} but produces a simple bar line"
               " (@code{|}) instead of a double bar line (@code{||}) at line breaks."
               " @code{S|} produces the segno sign at line breaks and starts the following"
               " line without special bar lines.\n"
               "\n"
               "@code{S|:} and @code{:|S} are used for repeat/segno combinations that are"
               " separated at line breaks.  Alternatively, @code{.S|:} and @code{:|S.}"
               " may be used which combine repeat signs and segno at the same line in"
               " case of a line break.  @code{:|S|:} is a combination of a left repeat"
               " (@code{:|}), a segno (@code{S}) and a right repeat @code{|:} which"
               " splits before the segno at line breaks; @code{:|S.|:} splits after"
               " the segno sign.\n"
               "\n"
               "If @var{bartype} is set to @code{empty} then nothing is"
               " printed, but a line break is allowed at that spot.\n"
               "\n"
               "@code{gap} is used for the gaps in dashed bar lines.",

               /* properties */
               "allow-span-bar "
               "gap "
               "kern "
               "thin-kern "
               "hair-thickness "
               "thick-thickness "
               "glyph "
               "glyph-name "
               "bar-extent "
              );
