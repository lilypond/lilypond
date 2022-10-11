/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>,
  Pal Benko <benkop@freestart.hu>

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

#include "mensural-ligature.hh"

#include "directional-element-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

using std::string;

/*
  draws one half a flexa, i.e. a portion corresponding to a single note.
  this way coloration of the two notes building up the flexa can be
  handled independently.

 * TODO: divide this function into mensural and neo-mensural style.
 *
 * TODO: move this function to class Lookup?
 */
Stencil
brew_flexa (Grob *me, bool solid, Real width, Real thickness, bool begin)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  /*
    The thickness of the horizontal lines of the flexa shape
    should be equal to that of the horizontal lines of the
    neomensural brevis note head (see mf/parmesan-heads.mf).
  */
  Real const horizontal_line_thickness = staff_space * 0.35;

  // URGH!  vertical_line_thickness is adjustable (via thickness
  // property), while horizontal_line_thickness is constant.
  // Maybe both should be adjustable independently?

  Real height = staff_space - horizontal_line_thickness;
  Stencil stencil;
  Real const interval
    = from_scm<double> (get_property (me, "flexa-interval"), 0.0);
  Real slope = (interval / 2.0 * staff_space) / width;

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to slope.
  Real ypos_correction = -0.1 * staff_space * sign (slope);
  Real slope_correction = 0.2 * staff_space * sign (slope);
  Real corrected_slope = slope + slope_correction / width;
  Real blotdiameter
    = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));
  width += 2 * blotdiameter;

  if (solid) // colorated flexae
    {
      stencil = Lookup::beam (corrected_slope, width * 0.5, staff_space,
                              blotdiameter);
    }
  else // outline
    {
      stencil = Lookup::beam (corrected_slope, thickness, height, blotdiameter);
      if (!begin)
        {
          stencil.translate_axis (width * 0.5 - thickness, X_AXIS);
          stencil.translate_axis (corrected_slope * (width * 0.5 - thickness),
                                  Y_AXIS);
        }

      Stencil bottom_edge = Lookup::beam (
        corrected_slope, width * 0.5, horizontal_line_thickness, blotdiameter);
      bottom_edge.translate_axis (-0.5 * height, Y_AXIS);
      stencil.add_stencil (bottom_edge);

      Stencil top_edge = Lookup::beam (corrected_slope, width * 0.5,
                                       horizontal_line_thickness, blotdiameter);
      top_edge.translate_axis (+0.5 * height, Y_AXIS);
      stencil.add_stencil (top_edge);
    }

  if (begin)
    stencil.translate_axis (ypos_correction, Y_AXIS);
  else
    {
      stencil.translate_axis (0.5 * thickness - blotdiameter, X_AXIS);

      stencil.translate_axis (interval / -4.0 * staff_space, Y_AXIS);
    }

  stencil.translate_axis (-thickness, X_AXIS);

  return stencil;
}

Stencil
internal_brew_primitive (Grob *me)
{
  SCM primitive_scm = get_property (me, "primitive");
  if (scm_is_null (primitive_scm))
    {
      programming_error ("Mensural_ligature:"
                         " undefined primitive -> ignoring grob");
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }
  int primitive = from_scm<int> (primitive_scm);

  Real thickness = 0.0;
  Real width = 0.0;
  Real flexa_width = 0.0;
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  SCM style = get_property (me, "style");
  bool const black = scm_is_eq (style, ly_symbol2scm ("blackpetrucci"));
  bool const semi = scm_is_eq (style, ly_symbol2scm ("semipetrucci"));

  if (primitive & MLP_ANY)
    {
      thickness = from_scm<double> (get_property (me, "thickness"), .13);
      width = from_scm<double> (get_property (me, "head-width"), staff_space)
              - thickness;
    }
  if (primitive & MLP_FLEXA)
    flexa_width
      = from_scm<double> (get_property (me, "flexa-width"), 2.0) * staff_space;

  Stencil out;
  int const note_shape = primitive & MLP_ANY;
  int duration_log = 0;
  Font_metric *fm = Font_interface::get_default_font (me);
  string prefix = "noteheads.";
  string index;
  string suffix;
  string color = "";
  if (black)
    color = "black";
  if (semi)
    color = "semi";

  switch (note_shape)
    {
    case MLP_NONE:
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    case MLP_MAXIMA:
      duration_log--;
    // fallthrough
    case MLP_LONGA:
      duration_log--;
    // fallthrough
    case MLP_BREVIS:
      duration_log--;
      suffix = std::to_string (duration_log) + color
               + (duration_log < -1 ? "lig" : "") + "mensural";
      index = prefix + "s";
      out = fm->find_by_name (index + "r" + suffix);
      if (!out.is_empty ()
          && !Staff_symbol_referencer::on_line (
            me, from_scm (get_property (me, "staff-position"), 0)))
        index += "r";
      out = fm->find_by_name (index + suffix);
      break;
    case MLP_FLEXA_BEGIN:
    case MLP_FLEXA_END:
      out = brew_flexa (me, black, flexa_width, thickness,
                        note_shape == MLP_FLEXA_BEGIN);
      break;
    default:
      programming_error ("Mensural_ligature:"
                         " unexpected case fall-through");
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }

  /*
    we use thickness because the stem end of the glyph
    "noteheads.sM2ligmensural" is round.
  */
  Real blotdiameter = thickness;
  /*
    instead of 2.5 the length of a longa stem should be used
    Font_interface::get_default_font (???)->find_by_name
    ("noteheads.sM2ligmensural").extent (Y_AXIS).length () * 0.5
  */
  Real stem_length = 2.5 * staff_space;

  if (primitive & MLP_STEM)
    {
      // assume MLP_UP
      Real y_bottom = 0.0, y_top = stem_length;

      if (primitive & MLP_DOWN)
        {
          y_bottom = -y_top;
          y_top = 0.0;
        }

      Interval x_extent (-thickness, 0);
      Interval y_extent (y_bottom, y_top);
      Box join_box (x_extent, y_extent);

      Stencil join = Lookup::round_filled_box (join_box, blotdiameter);
      out.add_stencil (join);
    }

  if (from_scm<bool> (get_property (me, "add-join")))
    {
      int join_right = from_scm<int> (get_property (me, "delta-position"));
      if (join_right)
        {
          Real y_top = join_right * 0.5 * staff_space;
          Real y_bottom = 0.0;

          if (y_top < 0.0)
            {
              y_bottom = y_top;
              y_top = 0.0;

              /*
                if the previous note is longa-shaped,
                the joining line may hide the stem, so made it longer
                to serve as stem as well
              */
              if (primitive & MLP_LONGA)
                y_bottom -= stem_length + 0.25 * blotdiameter;
            }

          Interval x_extent (width - thickness, width);
          Interval y_extent (y_bottom, y_top);
          Box join_box (x_extent, y_extent);
          Stencil join = Lookup::round_filled_box (join_box, blotdiameter);

          out.add_stencil (join);
        }
      else
        programming_error ("Mensural_ligature: (join_right == 0)");
    }

#if 0 /* what happend with the ledger lines? */
  int pos = Staff_symbol_referencer::get_rounded_position (me);
  if (primitive & MLP_FLEXA)
    {
      pos += delta_pitch;
      add_ledger_lines (me, &out, pos, 0.5 * delta_pitch, ledger_take_space);
    }
#endif

  return out;
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, brew_ligature_primitive,
                      "ly:mensural-ligature::brew-ligature-primitive", 1);
SCM
Mensural_ligature::brew_ligature_primitive (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);
  return internal_brew_primitive (me).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, print, "ly:mensural-ligature::print",
                      1);
SCM
Mensural_ligature::print (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Mensural_ligature,
               R"(
A mensural ligature.
               )",

               /* properties */
               R"(
delta-position
ligature-flexa
head-width
add-join
flexa-interval
primitive
thickness
               )");
