/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "rest.hh"

#include "directional-element-interface.hh"
#include "dots.hh"
#include "font-interface.hh"
#include "international.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "staff-symbol-referencer.hh"
#include "stencil.hh"
#include "grob.hh"

// -> offset callback
MAKE_SCHEME_CALLBACK (Rest, y_offset_callback, 1);
SCM
Rest::y_offset_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int duration_log = scm_to_int (me->get_property ("duration-log"));
  int line_count = Staff_symbol_referencer::line_count (me);
  Real ss = Staff_symbol_referencer::staff_space (me);

  bool position_override = scm_is_number (me->get_property ("staff-position"));
  Real amount = robust_scm2double (me->get_property ("staff-position"), 0)
    * 0.5 * ss;
  
  if (line_count % 2)
    {
      if (duration_log == 0 && line_count > 1)
	amount += ss;
    }
  else
    amount += ss / 2;

  if (!position_override)
    amount += 2 * ss * get_grob_direction (me);; 
  
  return scm_from_double (amount);
}

/* A rest might lie under a beam, in which case it should be cross-staff if
   the beam is cross-staff because the rest's position depends on the
   formatting of the beam. */
MAKE_SCHEME_CALLBACK (Rest, calc_cross_staff, 1);
SCM
Rest::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *stem = unsmob_grob (me->get_object ("stem"));

  if (!stem)
    return SCM_BOOL_F;

  return stem->get_property ("cross-staff");
}

/*
  make this function easily usable in C++
*/
string
Rest::glyph_name (Grob *me, int balltype, string style, bool try_ledgers)
{
  bool is_ledgered = false;
  if (try_ledgers && (balltype == 0 || balltype == 1))
    {
      Real rad = Staff_symbol_referencer::staff_radius (me) * 2.0;
      Real pos = Staff_symbol_referencer::get_position (me);

      /*
	Figure out when the rest is far enough outside the staff. This
	could bemore generic, but hey, we understand this even after
	dinner.
      */
      is_ledgered |= (balltype == 0) && (pos >= +rad + 2 || pos < -rad);
      is_ledgered |= (balltype == 1) && (pos <= -rad - 2 || pos > +rad);
    }

  string actual_style (style.c_str ());

  if ((style == "mensural") || (style == "neomensural"))
    {

      /*
	FIXME: Currently, ancient font does not provide ledgered rests;
	hence the "o" suffix in the glyph name is bogus.  But do we need
	ledgered rests at all now that we can draw ledger lines with
	variable width, length and blotdiameter? -- jr
      */
      is_ledgered = 0;

      /*
	There are no 32th/64th/128th mensural/neomensural rests.  In
	these cases, revert back to default style.
      */
      if (balltype > 4)
	actual_style = "";
    }

  if ((style == "classical") && (balltype != 2))
    {
      /*
	classical style: revert back to default style for any rest other
	than quarter rest
      */
      actual_style = "";
    }

  if (style == "default")
    {
      /*
	Some parts of lily still prefer style "default" over "".
	Correct this here. -- jr
      */
      actual_style = "";
    }

  return ("rests." + to_string (balltype) + (is_ledgered ? "o" : "")
	  + actual_style);
}

MAKE_SCHEME_CALLBACK (Rest, print, 1);
SCM
Rest::brew_internal_stencil (Grob *me, bool ledgered)
{
  SCM balltype_scm = me->get_property ("duration-log");
  if (!scm_is_number (balltype_scm))
    return Stencil ().smobbed_copy ();

  int balltype = scm_to_int (balltype_scm);

  string style;
  SCM style_scm = me->get_property ("style");
  if (scm_is_symbol (style_scm))
    style = ly_scm2string (scm_symbol_to_string (style_scm));

  Font_metric *fm = Font_interface::get_default_font (me);
  string font_char = glyph_name (me, balltype, style, ledgered);
  Stencil out = fm->find_by_name (font_char);
  if (out.is_empty ())
    me->warning (_f ("rest `%s' not found", font_char.c_str ()));

  return out.smobbed_copy ();
}

SCM
Rest::print (SCM smob)
{
  return brew_internal_stencil (unsmob_grob (smob), true);
}

MAKE_SCHEME_CALLBACK (Rest, width, 1);
/*
  We need the callback. The real stencil has ledgers depending on
  Y-position. The Y-position is known only after line breaking.  */
SCM
Rest::width (SCM smob)
{
  return generic_extent_callback (unsmob_grob (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Rest, height, 1);
SCM
Rest::height (SCM smob)
{
  return generic_extent_callback (unsmob_grob (smob), Y_AXIS);
}

/*
  We need the callback. The real stencil has ledgers depending on
  Y-position. The Y-position is known only after line breaking.  */
SCM
Rest::generic_extent_callback (Grob *me, Axis a)
{
  /*
    Don't want ledgers: ledgers depend on Y position, which depends on
    rest collision, which depends on stem size which depends on beam
    slop of opposite note column.

    consequence: we get too small extents and potential collisions
    with ledgered rests.
  */
  SCM m = brew_internal_stencil (me, a != X_AXIS);
  return ly_interval2scm (unsmob_stencil (m)->extent (a));
}

MAKE_SCHEME_CALLBACK (Rest, pure_height, 3);
SCM
Rest::pure_height (SCM smob,
		   SCM /* start */,
		   SCM /* end */)
{
  Grob *me = unsmob_grob (smob);
  SCM m = brew_internal_stencil (me, false);
  return ly_interval2scm (unsmob_stencil (m)->extent (Y_AXIS));
}

ADD_INTERFACE (Rest,
	       "A rest symbol.  The property @code{style} can be"
	       " @code{default}, @code{mensural}, @code{neomensural} or"
	       " @code{classical}.",

	       /* properties */
	       "direction "
	       "minimum-distance "
	       "style "
	       );

