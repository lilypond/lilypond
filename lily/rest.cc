/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "staff-symbol.hh"
#include "stencil.hh"
#include "grob.hh"

// -> offset callback
MAKE_SCHEME_CALLBACK (Rest, y_offset_callback, 1);
SCM
Rest::y_offset_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  int duration_log = scm_to_int (me->get_property ("duration-log"));
  Real ss = Staff_symbol_referencer::staff_space (me);

  bool position_override = scm_is_number (me->get_property ("staff-position"));
  Real amount;

  if (position_override)
    {
      amount
        = robust_scm2double (me->get_property ("staff-position"), 0) * 0.5 * ss;

      /*
        semibreve rests are positioned one staff line off
      */
      if (duration_log == 0)
        amount += ss;

      /*
        trust the client on good positioning;
        would be tempting to adjust position of rests longer than a quarter
        to be properly aligned to staff lines,
        but custom rest shapes may not need that sort of care.
      */
    }
  else
    {
      int pos = 4 * get_grob_direction (me);

      /*
        make a semibreve rest hang from the next line,
        except for a single line staff
      */
      if (duration_log == 0 && Staff_symbol_referencer::line_count (me) > 1)
        pos += 2;

      /*
        make sure rest is aligned to a staff line
      */
      if (Grob *staff = Staff_symbol_referencer::get_staff_symbol (me))
        {
          std::vector<Real> linepos = Staff_symbol::line_positions (staff);
          std::sort (linepos.begin (), linepos.end ());
          std::vector<Real>::const_iterator it
            = std::lower_bound (linepos.begin (), linepos.end (), pos);
          if (it != linepos.end ())
            {
              pos = (int)ceil (*it);
            }
        }

      amount = ss * 0.5 * pos;
    }

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
Rest::glyph_name (Grob *me, int durlog, string style, bool try_ledgers)
{
  bool is_ledgered = false;
  if (try_ledgers && (durlog == -1 || durlog == 0 || durlog == 1))
    {
      int const pos = int (Staff_symbol_referencer::get_position (me));

      /*
        half rests need ledger if not lying on a staff line,
        whole rests need ledger if not hanging from a staff line,
        breve rests need ledger if neither lying on nor hanging from a staff line
      */
      if (-1 <= durlog && durlog <= 1)
        is_ledgered = !Staff_symbol_referencer::on_staff_line (me, pos)
                      && !(durlog == -1
                           && Staff_symbol_referencer::on_staff_line (me, pos + 2));
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
      if (durlog > 4)
        actual_style = "";
    }

  if ((style == "classical") && (durlog != 2))
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

  return ("rests." + to_string (durlog) + (is_ledgered ? "o" : "")
          + actual_style);
}

MAKE_SCHEME_CALLBACK (Rest, print, 1);
SCM
Rest::brew_internal_stencil (Grob *me, bool ledgered)
{
  SCM durlog_scm = me->get_property ("duration-log");
  if (!scm_is_number (durlog_scm))
    return Stencil ().smobbed_copy ();

  int durlog = scm_to_int (durlog_scm);

  string style = robust_symbol2string (me->get_property ("style"), "default");

  Font_metric *fm = Font_interface::get_default_font (me);
  string font_char = glyph_name (me, durlog, style, ledgered);
  Stencil out = fm->find_by_name (font_char);
  if (out.is_empty ())
    me->warning (_f ("rest `%s' not found", font_char.c_str ()));

  return out.smobbed_copy ();
}

/**
   translate the rest vertically by amount DY, but only if
   it doesn't have staff-position set.
*/
void
Rest::translate (Grob *me, int dy)
{
  if (!scm_is_number (me->get_property ("staff-position")))
    {
      me->translate_axis (dy * Staff_symbol_referencer::staff_space (me) / 2.0, Y_AXIS);
      Grob *p = me->get_parent (Y_AXIS);
      p->flush_extent_cache (Y_AXIS);
    }
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
