/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Michael Krause
  Extensions for ancient notation (c) 2003--2022 by Juergen Reuter

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

#include "breathing-sign.hh"

#include "dimensions.hh"
#include "direction.hh"
#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol.hh"
#include "staff-symbol-referencer.hh"
#include "text-interface.hh"

/*
  UGH : this is full of C&P code. Consolidate!  --hwn
*/

/*
  Gregorian chant divisio minima.  (Actually, this was the original
  breathing sign by Michael. -- jr)
*/
MAKE_SCHEME_CALLBACK (Breathing_sign, divisio_minima, 1);
SCM
Breathing_sign::divisio_minima (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= from_scm<double> (get_property (me, "thickness"), 1.0);

  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  /*
   * Draw a small vertical line through the uppermost (or, depending
   * on direction, lowermost) staff line.
   */
  Interval xdim (0, thickness);
  Interval ydim (-0.5 * staff_space, +0.5 * staff_space);
  Box b (xdim, ydim);
  Stencil out = Lookup::round_filled_box (b, blotdiameter);
  return out.smobbed_copy ();
}

/*
  Gregorian chant divisio maior.
*/
MAKE_SCHEME_CALLBACK (Breathing_sign, divisio_maior, 1);
SCM
Breathing_sign::divisio_maior (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= from_scm<double> (get_property (me, "thickness"), 1.0);

  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  /*
    Draw a vertical line that is roughly centered vertically in
    the staff (just like a bar) with the following requirements:
    1. length should be at least half the size of the staff
    2. both ends should be in the middle of a staff space.

    These two requirements contradict if the first or last space is
    larger than half of the whole staff (e.g. the staff consists of
    two lines only); in such cases the first prescription wins.
  */
  Interval ydim (0.0, 0.0);
  if (Grob *staff = Staff_symbol_referencer::get_staff_symbol (me))
    {
      std::vector<Real> line_pos = Staff_symbol::line_positions (staff);
      if (!line_pos.empty ())
        {
          std::sort (line_pos.begin (), line_pos.end ());
          ydim[DOWN] = line_pos.front ();
          ydim[UP] = line_pos.back ();
          if (Real const height = ydim.length ())
            {
              ydim.widen (-0.25 * height);

              /*
                ydim has now the required height; to satisfy req. 2
                find the staff spaces containing current endpoints.

                standard algorithms are suitable to find the upper
                line of these spaces; we must choose between
                upper_bound and lower_bound considering that if
                there's a line exactly at quarter of the staff (the
                lower end) then we need the space below it, while if
                there's a line exactly at three quarters of the staff
                (upper end) then we need the space above it.

                if the middle of the space found is not low/high
                enough, take the next space (if there are no more
                spaces, ydim won't be enlarged further).
              */
              std::vector<Real>::const_iterator it
                = std::lower_bound (line_pos.begin (), line_pos.end (),
                                    ydim[DOWN]);
              assert (line_pos.begin () < it);
              double val = (it[-1] + it[0]) / 2;
              if (ydim[DOWN] < val && line_pos.begin () < it - 1)
                val = (it[-2] + it[-1]) / 2;
              ydim.add_point (val);

              it = std::upper_bound (line_pos.begin (), line_pos.end (),
                                     ydim[UP]);
              assert (it < line_pos.end ());
              val = (it[-1] + it[0]) / 2;
              if (val < ydim[UP] && it + 1 < line_pos.end ())
                val = (it[0] + it[1]) / 2;
              ydim.add_point (val);
            }
        }
    }

  ydim *= Staff_symbol_referencer::staff_space (me) / 2;

  Interval xdim (0, thickness);
  Box b (xdim, ydim);
  Stencil out = Lookup::round_filled_box (b, blotdiameter);
  return out.smobbed_copy ();
}

/*
  Gregorian chant divisio maxima.
*/
MAKE_SCHEME_CALLBACK (Breathing_sign, divisio_maxima, 1);
SCM
Breathing_sign::divisio_maxima (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= from_scm<double> (get_property (me, "thickness"), 1.0);

  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  // like a "|" type bar
  Interval xdim (0, thickness);
  Interval ydim = Staff_symbol_referencer::staff_span (me);
  ydim *= staff_space / 2;
  Box b (xdim, ydim);
  Stencil out = Lookup::round_filled_box (b, blotdiameter);
  return out.smobbed_copy ();
}

/*
  Gregorian chant finalis.
*/
MAKE_SCHEME_CALLBACK (Breathing_sign, finalis, 1);
SCM
Breathing_sign::finalis (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= from_scm<double> (get_property (me, "thickness"), 1.0);

  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  // like a "||" type bar
  Interval xdim (0, thickness);
  Interval ydim = Staff_symbol_referencer::staff_span (me);
  ydim *= staff_space / 2;
  Box b (xdim, ydim);
  Stencil line1 = Lookup::round_filled_box (b, blotdiameter);
  Stencil line2 (line1);
  line2.translate_axis (0.5 * staff_space, X_AXIS);
  line1.add_stencil (line2);

  return line1.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Breathing_sign, offset_callback, 1);
SCM
Breathing_sign::offset_callback (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  Direction d = get_strict_grob_direction (me);

  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (staff)
    {
      Interval iv = Staff_symbol::line_span (staff);
      Real inter = Staff_symbol::staff_space (staff) / 2;
      return to_scm (inter * iv[d]);
    }

  return to_scm (0.0);
}

ADD_INTERFACE (Breathing_sign,
               R"(
A breathing sign.
               )",

               /* properties */
               R"(
direction
thickness
               )");
