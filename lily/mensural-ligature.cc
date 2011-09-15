/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2011 Juergen Reuter <reuter@ipd.uka.de>,
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

#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

/*
  draws one half a flexa, i.e. a portion corresponding to a single note.
  this way coloration of the two notes building up the flexa can be
  handled independently.

 * TODO: divide this function into mensural and neo-mensural style.
 *
 * TODO: move this function to class Lookup?
 */
Stencil
brew_flexa (Grob *me,
            bool solid,
            Real width,
            Real thickness,
            bool begin)
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
    = robust_scm2double (me->get_property ("flexa-interval"), 0.0);
  Real slope = (interval / 2.0 * staff_space) / width;

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to slope.
  Real ypos_correction = -0.1 * staff_space * sign (slope);
  Real slope_correction = 0.2 * staff_space * sign (slope);
  Real corrected_slope = slope + slope_correction / width;

  if (solid) // colorated flexae
    {
      stencil = Lookup::beam (corrected_slope, width * 0.5, staff_space, 0.0);
    }
  else // outline
    {
      stencil = Lookup::beam (corrected_slope, thickness, height, 0.0);
      if (!begin)
        {
          stencil.translate_axis (width * 0.5 - thickness, X_AXIS);
          stencil.translate_axis (corrected_slope * (width * 0.5 - thickness),
                                  Y_AXIS);
        }

      Stencil bottom_edge
        = Lookup::beam (corrected_slope, width * 0.5, horizontal_line_thickness,
                        0.0);
      bottom_edge.translate_axis (-0.5 * height, Y_AXIS);
      stencil.add_stencil (bottom_edge);

      Stencil top_edge
        = Lookup::beam (corrected_slope, width * 0.5, horizontal_line_thickness,
                        0.0);
      top_edge.translate_axis (+0.5 * height, Y_AXIS);
      stencil.add_stencil (top_edge);
    }

  if (begin)
    stencil.translate_axis (ypos_correction, Y_AXIS);
  else
    {
      stencil.translate_axis (0.5 * thickness, X_AXIS);

      stencil.translate_axis (interval / -4.0 * staff_space, Y_AXIS);
    }

  return stencil;
}

Stencil
internal_brew_primitive (Grob *me)
{
  SCM primitive_scm = me->get_property ("primitive");
  if (primitive_scm == SCM_EOL)
    {
      programming_error ("Mensural_ligature:"
                         " undefined primitive -> ignoring grob");
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }
  int primitive = scm_to_int (primitive_scm);

  Stencil out;
  Real thickness = 0.0;
  Real width = 0.0;
  Real flexa_width = 0.0;
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  bool const color
    = me->get_property ("style") == ly_symbol2scm ("blackpetrucci");
  bool const semi
    = me->get_property ("style") == ly_symbol2scm ("semipetrucci");

  if (primitive & MLP_ANY)
    {
      thickness = robust_scm2double (me->get_property ("thickness"), .14);
      width = robust_scm2double (me->get_property ("head-width"), staff_space);
    }
  if (primitive & MLP_FLEXA)
    flexa_width = robust_scm2double (me->get_property ("flexa-width"), 2.0)
                  * staff_space;

  int const note_shape = primitive & MLP_ANY;

  switch (note_shape)
    {
    case MLP_NONE:
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    case MLP_LONGA: // mensural brevis head with right cauda
      out = Font_interface::get_default_font (me)->find_by_name
            (color ? "noteheads.sM2blackmensural"
             : semi ? "noteheads.sM2semimensural" : "noteheads.sM2mensural");
      break;
    case MLP_BREVIS: // mensural brevis head
      out = Font_interface::get_default_font (me)->find_by_name
            (color ? "noteheads.sM1blackmensural"
             : semi ? "noteheads.sM1semimensural" : "noteheads.sM1mensural");
      break;
    case MLP_MAXIMA: // should be mensural maxima head without stem
      out = Font_interface::get_default_font (me)->find_by_name
            (color ? "noteheads.sM3blackligmensural"
             : semi ? "noteheads.sM3semiligmensural" : "noteheads.sM3ligmensural");
      break;
    case MLP_FLEXA_BEGIN:
    case MLP_FLEXA_END:
      out = brew_flexa (me, color, flexa_width, thickness,
                        note_shape == MLP_FLEXA_BEGIN);
      break;
    default:
      programming_error ("Mensural_ligature:"
                         " unexpected case fall-through");
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }

  Real blotdiameter
    = (me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter")));

  if (primitive & MLP_STEM)
    {
      // assume MLP_UP
      Real y_bottom = 0.0, y_top = 3.0 * staff_space;

      if (primitive & MLP_DOWN)
        {
          y_bottom = -y_top;
          y_top = 0.0;
        }

      Interval x_extent (0, thickness);
      Interval y_extent (y_bottom, y_top);
      Box join_box (x_extent, y_extent);

      Stencil join = Lookup::round_filled_box (join_box, blotdiameter);
      out.add_stencil (join);
    }

  if (to_boolean (me->get_property ("add-join")))
    {
      int join_right = scm_to_int (me->get_property ("delta-position"));
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
                /*
                  instead of 3.0 the length of a longa stem should be used
                  Font_interface::get_default_font (???)->find_by_name
                  ("noteheads.s-2mensural").extent (Y_AXIS).length () * 0.5
                */
                y_bottom -= 3.0 * staff_space;
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

MAKE_SCHEME_CALLBACK (Mensural_ligature, brew_ligature_primitive, 1);
SCM
Mensural_ligature::brew_ligature_primitive (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return internal_brew_primitive (me).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, print, 1);
SCM
Mensural_ligature::print (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Mensural_ligature,
               "A mensural ligature.",

               /* properties */
               "delta-position "
               "ligature-flexa "
               "head-width "
               "add-join "
               "flexa-interval "
               "primitive "
               "thickness "
              );
