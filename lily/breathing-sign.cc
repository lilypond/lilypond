/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2012 Michael Krause
  Extensions for ancient notation (c) 2003--2012 by Juergen Reuter

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
  Grob *me = unsmob_grob (smob);
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= robust_scm2double (me->get_property ("thickness"), 1.0);

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
  Grob *me = unsmob_grob (smob);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= robust_scm2double (me->get_property ("thickness"), 1.0);

  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  /*
   * Draw a vertical line that is vertically centered in the staff
   * (just like a bar).  The height of this line should be a little
   * more than half the size of the staff, such that the endings of
   * the line are in the middle of a staff space.
   */
  Interval ydim = Staff_symbol_referencer::staff_span (me);
  ydim.widen (-0.25 * ydim.delta ());
  for (UP_and_DOWN (i))
    {
      int const int_dim = (int) ydim[i];
      if (int_dim == ydim[i]
          && Staff_symbol_referencer::on_staff_line (me, int_dim))
        ydim[i] += i;
    }

  ydim *= 1.0 / Staff_symbol_referencer::staff_space (me);

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
  Grob *me = unsmob_grob (smob);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= robust_scm2double (me->get_property ("thickness"), 1.0);

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
  Grob *me = unsmob_grob (smob);
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real thickness = Staff_symbol_referencer::line_thickness (me);
  thickness *= robust_scm2double (me->get_property ("thickness"), 1.0);

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
  Grob *me = unsmob_grob (smob);

  Direction d = get_grob_direction (me);
  if (!d)
    {
      d = UP;
      set_grob_direction (me, d);
    }

  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (staff)
    {
      Interval iv = Staff_symbol::line_span (staff);
      Real inter = Staff_symbol::staff_space (staff) / 2;
      return scm_from_double (inter * iv[d]);
    }

  return scm_from_double (0.0);
}

ADD_INTERFACE (Breathing_sign,
               "A breathing sign.",

               /* properties */
               "direction "
              );
