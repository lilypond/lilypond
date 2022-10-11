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

#include "rest.hh"

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "stencil.hh"
#include "grob.hh"

using std::string;

// -> offset callback
MAKE_SCHEME_CALLBACK (Rest, y_offset_callback, "ly:rest::y-offset-callback", 1);
SCM
Rest::y_offset_callback (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  int duration_log = from_scm<int> (get_property (me, "duration-log"));
  Real ss = Staff_symbol_referencer::staff_space (me);

  return to_scm (ss * 0.5
                 * Rest::staff_position_internal (me, duration_log,
                                                  get_grob_direction (me)));
}

Real
Rest::staff_position_internal (Grob *me, int duration_log, Direction dir)
{
  if (!me)
    return 0;

  bool position_override = scm_is_number (get_property (me, "staff-position"));
  Real pos;

  if (position_override)
    {
      pos = from_scm<double> (get_property (me, "staff-position"), 0);

      /*
        semibreve rests are positioned one staff line off
      */
      if (duration_log == 0)
        return pos + 2;

      /*
        trust the client on good positioning;
        would be tempting to adjust position of rests longer than a quarter
        to be properly aligned to staff lines,
        but custom rest shapes may not need that sort of care.
      */

      return pos;
    }

  Real vpos = dir * from_scm (get_property (me, "voiced-position"), 0);
  pos = vpos;

  if (duration_log > 1)
    /* Only half notes or longer want alignment with staff lines */
    return pos;

  /*
    We need a staff symbol for actually aligning anything
  */
  Grob *staff = Staff_symbol_referencer::get_staff_symbol (me);
  if (!staff)
    return pos;

  std::vector<Real> linepos = Staff_symbol::line_positions (staff);

  if (linepos.empty ())
    return pos;

  if (linepos.size () == 1 && duration_log < 0 && !get_grob_direction (me))
    return linepos[0] - 2;

  std::sort (linepos.begin (), linepos.end ());

  if (duration_log == 0)
    {
      /*
        lower voice semibreve rests generally hang a line lower
      */

      if (dir < CENTER)
        pos -= 2;

      /*
        make a semibreve rest hang from the next available line,
        except when there is none.
      */

      std::vector<Real>::const_iterator it
        = std::upper_bound (linepos.begin (), linepos.end (), pos);
      if (it != linepos.end ())
        pos = *it;
      else
        pos = linepos.back ();
    }
  else
    {
      std::vector<Real>::const_iterator it
        = std::upper_bound (linepos.begin (), linepos.end (), pos);
      if (it != linepos.begin ())
        --it;
      pos = *it;
    }

  /* Finished for neutral position */
  if (!dir)
    return pos;

  /* If we have a voiced position, make sure that it's on the
     proper side of neutral before using it.
  */

  Real neutral = staff_position_internal (me, duration_log, CENTER);

  if (dir * (pos - neutral) > 0)
    return pos;
  else
    return neutral + vpos;
}

/* A rest might lie under a beam, in which case it should be cross-staff if
   the beam is cross-staff because the rest's position depends on the
   formatting of the beam. */
MAKE_SCHEME_CALLBACK (Rest, calc_cross_staff, "ly:rest::calc-cross-staff", 1);
SCM
Rest::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  Grob *stem = unsmob<Grob> (get_object (me, "stem"));

  if (!stem)
    return SCM_BOOL_F;

  return get_property (stem, "cross-staff");
}

/*
  make this function easily usable in C++
*/
string
Rest::glyph_name (Grob *me, int durlog, const string &style, bool try_ledgers,
                  Real offset)
{
  bool is_ledgered = false;
  if (try_ledgers && (durlog == -1 || durlog == 0 || durlog == 1))
    {
      int const pos = int (Staff_symbol_referencer::get_position (me) + offset);
      /*
        half rests need ledger if not lying on a staff line,
        whole rests need ledger if not hanging from a staff line,
        breve rests need ledger if neither lying on nor hanging from a staff line
      */
      if (-1 <= durlog && durlog <= 1)
        is_ledgered
          = !Staff_symbol_referencer::on_staff_line (me, pos)
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

  if (((style == "classical") || (style == "z")) && (durlog != 2))
    {
      /*
        these styles differ from the default in quarter rests only
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

  return ("rests." + std::to_string (durlog) + (is_ledgered ? "o" : "")
          + actual_style);
}

MAKE_SCHEME_CALLBACK (Rest, print, "ly:rest::print", 1);
SCM
Rest::brew_internal_stencil (Grob *me, bool ledgered)
{
  SCM durlog_scm = get_property (me, "duration-log");
  if (!scm_is_number (durlog_scm))
    return Stencil ().smobbed_copy ();

  int durlog = from_scm<int> (durlog_scm);

  string style = robust_symbol2string (get_property (me, "style"), "default");

  Font_metric *fm = Font_interface::get_default_font (me);
  string font_char = glyph_name (me, durlog, style, ledgered, 0.0);
  Stencil out = fm->find_by_name (font_char);
  if (out.is_empty ())
    me->warning (_f ("rest `%s' not found", font_char.c_str ()));

  if (durlog < 0)
    {
      Real fs
        = pow (2, from_scm<double> (get_property (me, "font-size"), 0) / 6);
      Real ss = Staff_symbol_referencer::staff_space (me);
      out.translate_axis (ss - fs, Y_AXIS);
    }

  return out.smobbed_copy ();
}

/**
   translate the rest vertically by amount DY, but only if
   it doesn't have staff-position set.
*/
void
Rest::translate (Grob *me, int dy)
{
  if (!scm_is_number (get_property (me, "staff-position")))
    {
      me->translate_axis (dy * Staff_symbol_referencer::staff_space (me) / 2.0,
                          Y_AXIS);
      Grob *p = me->get_y_parent ();
      p->flush_extent_cache (Y_AXIS);
    }
}

SCM
Rest::print (SCM smob)
{
  return brew_internal_stencil (unsmob<Grob> (smob), true);
}

MAKE_SCHEME_CALLBACK (Rest, width, "ly:rest::width", 1);
/*
  We need the callback. The real stencil has ledgers depending on
  Y-position. The Y-position is known only after line breaking.  */
SCM
Rest::width (SCM smob)
{
  return generic_extent_callback (unsmob<Grob> (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Rest, height, "ly:rest::height", 1);
SCM
Rest::height (SCM smob)
{
  return generic_extent_callback (unsmob<Grob> (smob), Y_AXIS);
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
  return to_scm (unsmob<const Stencil> (m)->extent (a));
}

MAKE_SCHEME_CALLBACK (Rest, pure_height, "ly:rest::pure-height", 3);
SCM
Rest::pure_height (SCM smob, SCM /* start */, SCM /* end */)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  SCM m = brew_internal_stencil (me, false);
  return to_scm (unsmob<const Stencil> (m)->extent (Y_AXIS));
}

ADD_INTERFACE (Rest,
               R"(
A rest symbol.  The property @code{style} can be @code{default},
@code{mensural}, @code{neomensural} or @code{classical}.
               )",

               /* properties */
               R"(
direction
minimum-distance
style
voiced-position
               )");
