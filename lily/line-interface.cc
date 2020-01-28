/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "line-interface.hh"

#include "font-interface.hh"
#include "grob.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"

using std::vector;

Stencil
Line_interface::make_arrow (Offset begin, Offset end, Real thick, Real length,
                            Real width)
{
  Offset dir = (end - begin).direction ();
  vector<Offset> points;

  points.push_back (Offset (0, 0));
  points.push_back (Offset (-length, width));
  points.push_back (Offset (-length, -width));

  for (vsize i = 0; i < points.size (); i++)
    points[i] = points[i] * dir + end;

  return Lookup::round_filled_polygon (points, thick);
}

Stencil
Line_interface::make_trill_line (Grob *me, Offset from, Offset to)
{
  Offset dz = (to - from);

  Font_metric *fm = Font_interface::get_default_font (me);

  Stencil elt = fm->find_by_name ("scripts.trill_element");
  elt.align_to (Y_AXIS, CENTER);
  Real elt_len = elt.extent (X_AXIS).length ();
  if (elt_len <= 0.0)
    {
      programming_error ("can't find scripts.trill_element");
      return Stencil ();
    }

  Stencil line;
  Real len = 0.0;
  do
    {
      line.add_at_edge (X_AXIS, RIGHT, elt, 0);
      len = line.extent (X_AXIS).length ();
    }
  while (len + elt_len < dz.length ());

  line.rotate (dz.angle_degrees (), Offset (LEFT, CENTER));
  line.translate (from);

  return line;
}

Stencil
Line_interface::make_zigzag_line (Grob *me, Offset from, Offset to)
{
  Offset dz = to - from;

  Real thick = Staff_symbol_referencer::line_thickness (me);
  thick *= robust_scm2double (me->get_property ("thickness"),
                              1.0); // todo: staff sym referencer?

  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real w
      = robust_scm2double (me->get_property ("zigzag-width"), 1) * staff_space;
  int count = (int)ceil (dz.length () / w);
  w = dz.length () / count;

  Real l = robust_scm2double (me->get_property ("zigzag-length"), 1) * w;
  Real h = l > w / 2 ? sqrt (l * l - w * w / 4) : 0;

  Offset rotation_factor = dz.direction ();

  Offset points[3];
  points[0] = Offset (0, -h / 2);
  points[1] = Offset (w / 2, h / 2);
  points[2] = Offset (w, -h / 2);
  for (int i = 0; i < 3; i++)
    points[i] = complex_multiply (points[i], rotation_factor);

  Stencil squiggle (Line_interface::make_line (thick, points[0], points[1]));
  squiggle.add_stencil (
      Line_interface::make_line (thick, points[1], points[2]));

  Stencil total;
  for (int i = 0; i < count; i++)
    {
      Stencil moved_squiggle (squiggle);
      moved_squiggle.translate (from + Offset (i * w, 0) * rotation_factor);
      total.add_stencil (moved_squiggle);
    }

  return total;
}

Stencil
Line_interface::make_dashed_line (Real thick, Offset from, Offset to,
                                  Real dash_period, Real dash_fraction)
{
  dash_fraction = std::min (std::max (dash_fraction, 0.0), 1.0);
  Real on = dash_fraction * dash_period;
  Real off = std::max (0.0, dash_period - on);

  SCM at = scm_list_n (ly_symbol2scm ("dashed-line"), scm_from_double (thick),
                       scm_from_double (on), scm_from_double (off),
                       scm_from_double (to[X_AXIS] - from[X_AXIS]),
                       scm_from_double (to[Y_AXIS] - from[Y_AXIS]),
                       scm_from_double (0.0), SCM_UNDEFINED);

  Box box;
  box.add_point (Offset (0, 0));
  box.add_point (to - from);

  box[X_AXIS].widen (thick / 2);
  box[Y_AXIS].widen (thick / 2);

  Stencil m = Stencil (box, at);
  m.translate (from);
  return m;
}

Stencil
Line_interface::make_line (Real th, Offset from, Offset to)
{
  SCM at = scm_list_n (ly_symbol2scm ("draw-line"), scm_from_double (th),
                       scm_from_double (from[X_AXIS]),
                       scm_from_double (from[Y_AXIS]),
                       scm_from_double (to[X_AXIS]),
                       scm_from_double (to[Y_AXIS]), SCM_UNDEFINED);

  Box box;
  box.add_point (from);
  box.add_point (to);

  box[X_AXIS].widen (th / 2);
  box[Y_AXIS].widen (th / 2);

  return Stencil (box, at);
}

Stencil
Line_interface::arrows (Grob *me, Offset from, Offset to, bool from_arrow,
                        bool to_arrow)
{
  Stencil a;
  if (from_arrow || to_arrow)
    {
      Real thick = Staff_symbol_referencer::line_thickness (me)
                   * robust_scm2double (me->get_property ("thickness"), 1);
      Real ss = Staff_symbol_referencer::staff_space (me);

      Real len
          = robust_scm2double (me->get_property ("arrow-length"), 1.3 * ss);
      Real wid = robust_scm2double (me->get_property ("arrow-width"), 0.5 * ss);

      if (to_arrow)
        a.add_stencil (make_arrow (from, to, thick, len, wid));

      if (from_arrow)
        a.add_stencil (make_arrow (to, from, thick, len, wid));
    }

  return a;
}

Stencil
Line_interface::line (Grob *me, Offset from, Offset to)
{
  Real thick = Staff_symbol_referencer::line_thickness (me)
               * robust_scm2double (me->get_property ("thickness"), 1);

  SCM type = me->get_property ("style");
  if (scm_is_eq (type, ly_symbol2scm ("zigzag")))
    return make_zigzag_line (me, from, to);
  else if (scm_is_eq (type, ly_symbol2scm ("trill")))
    return make_trill_line (me, from, to);
  else if (scm_is_eq (type, ly_symbol2scm ("none")))
    return Stencil ();

  Stencil stencil;

  if (scm_is_eq (type, ly_symbol2scm ("dashed-line"))
      || scm_is_eq (type, ly_symbol2scm ("dotted-line")))
    {

      Real fraction
          = scm_is_eq (type, ly_symbol2scm ("dotted-line"))
                ? 0.0
                : robust_scm2double (me->get_property ("dash-fraction"), 0.4);

      fraction = std::min (std::max (fraction, 0.0), 1.0);
      Real period = Staff_symbol_referencer::staff_space (me)
                    * robust_scm2double (me->get_property ("dash-period"), 1.0);

      if (period <= 0)
        return Stencil ();

      Real len = (to - from).length ();
      /*
        Dashed lines should begin and end with a dash.  Therefore,
        there will be one more dash than complete dash + whitespace
        units (full periods).
      */
      int full_period_count = (int)rint ((len - period * fraction) / period);
      full_period_count = std::max (0, full_period_count);
      if (full_period_count > 0)
        {
          /*
            TODO: figure out something intelligent for really short
            sections.
          */
          period = len / (fraction + full_period_count);
        }
      stencil = make_dashed_line (thick, from, to, period, fraction);
    }
  else
    stencil = make_line (thick, from, to);

  return stencil;
}

ADD_INTERFACE (Line_interface,
               "Generic line objects.  Any object using lines supports this."
               "  The property @code{style} can be @code{line},"
               " @code{dashed-line}, @code{trill}, @code{dotted-line},"
               " @code{zigzag} or @code{none} (a transparent line).\n"
               "\n"
               "For @code{dashed-line}, the length of the dashes is tuned"
               " with @code{dash-fraction}.  If the latter is set to@tie{}0, a"
               " dotted line is produced.",

               /* properties */
               "arrow-length "
               "arrow-width "
               "dash-fraction "
               "dash-period "
               "style "
               "thickness "
               "zigzag-length "
               "zigzag-width ");
