/*
  vaticana-ligature.cc -- implement Vaticana_ligature

  source file of the GNU LilyPond music typesetter

  (c) 2003--2009 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "vaticana-ligature.hh"

#include "bezier.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

Stencil
vaticana_brew_cauda (Grob *me,
		     int pos,
		     int delta_pitch,
		     Real thickness,
		     Real blotdiameter)
{
  bool on_staffline = Staff_symbol_referencer::on_line (me, pos);
  int interspaces = Staff_symbol_referencer::line_count (me) - 1;
  bool above_staff = pos > interspaces;

  if (delta_pitch > -1)
    {
      me->programming_error ("flexa cauda: invalid delta_pitch; assuming -1");
      delta_pitch = -1;
    }
  Real length;
  if (on_staffline)
    {
      if (delta_pitch >= -1)
	length = 1.30;
      else if (delta_pitch >= -2)
	length = 1.35;
      else
	length = 1.85;
    }
  else
    {
      if (delta_pitch >= -1)
	if (above_staff)
	  length = 1.30;
	else
	  length = 1.00;
      else if (delta_pitch >= -2)
	length = 1.35;
      else if (delta_pitch >= -3)
	length = 1.50;
      else
	length = 1.85;
    }
  Box cauda_box (Interval (0, thickness), Interval (-length, 0));
  return Lookup::round_filled_box (cauda_box, blotdiameter);
}

/*
 * TODO: move this function to class Lookup?
 */
Stencil
vaticana_brew_flexa (Grob *me,
		     bool solid,
		     Real line_thickness)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Stencil stencil;
  Real right_height = 0.6 * staff_space;

  Real interval;
  SCM flexa_height_scm = me->get_property ("flexa-height");
  if (flexa_height_scm != SCM_EOL)
    interval = scm_to_int (flexa_height_scm);
  else
    {
      me->warning ("Vaticana_ligature: "
		   + _ ("flexa-height undefined; assuming 0"));
      interval = 0.0;
    }

  if (interval >= 0.0)
    me->warning (_ ("ascending vaticana style flexa"));

  Real width = robust_scm2double (me->get_property ("flexa-width"), 2);

  /*
   * Compensate curve thickness that appears to be smaller in steep
   * section of bend.
   */
  Real left_height
    = right_height
    + min (0.12 * abs (interval), 0.3) * staff_space;

  /*
   * Compensate optical illusion regarding vertical position of left
   * and right endings due to curved shape.
   */
  Real ypos_correction = -0.1 * staff_space * sign (interval);
  Real interval_correction = 0.2 * staff_space * sign (interval);
  Real corrected_interval = interval * staff_space + interval_correction;

  /*
   * middle curve of flexa shape
   */
  Bezier curve;
  curve.control_[0] = Offset (0.00 * width, 0.0);
  curve.control_[1] = Offset (0.33 * width, corrected_interval / 2.0);
  curve.control_[2] = Offset (0.66 * width, corrected_interval / 2.0);
  curve.control_[3] = Offset (1.00 * width, corrected_interval / 2.0);

  Bezier top_curve = curve, bottom_curve = curve;
  for (int i = 0; i < 4; i++)
    {
      Real curve_thickness = 0.33 * ((3 - i) * left_height + i * right_height);
      top_curve.control_[i] += Offset (0, 0.5 * curve_thickness);
      bottom_curve.control_[i] -= Offset (0, 0.5 * curve_thickness);
    }

  if (solid)
    {
      Stencil solid_head
	= Lookup::bezier_sandwich (top_curve, bottom_curve);
      stencil.add_stencil (solid_head);
    }
  else // outline
    {
      Bezier inner_top_curve = top_curve;
      inner_top_curve.translate (Offset (0.0, -line_thickness));
      Stencil top_edge
	= Lookup::bezier_sandwich (top_curve, inner_top_curve);
      stencil.add_stencil (top_edge);

      Bezier inner_bottom_curve = bottom_curve;
      inner_bottom_curve.translate (Offset (0.0, +line_thickness));
      Stencil bottom_edge
	= Lookup::bezier_sandwich (bottom_curve, inner_bottom_curve);
      stencil.add_stencil (bottom_edge);

      /*
       * TODO: Use horizontal slope with proper slope value rather
       * than filled box for left edge, since the filled box stands
       * out from the flexa shape if the interval is big and the line
       * thickness small.  The difficulty here is to compute a proper
       * slope value, as it should roughly be equal with the slope of
       * the left end of the bezier curve.
       */
      Box left_edge_box (Interval (0, line_thickness),
			 Interval (-0.5 * left_height, +0.5 * left_height));
      Stencil left_edge = Lookup::filled_box (left_edge_box);
      stencil.add_stencil (left_edge);

      Box right_edge_box (Interval (-line_thickness, 0),
			  Interval (-0.5 * right_height, +0.5 * right_height));
      Stencil right_edge = Lookup::filled_box (right_edge_box);
      right_edge.translate_axis (width, X_AXIS);
      right_edge.translate_axis (corrected_interval / 2.0, Y_AXIS);
      stencil.add_stencil (right_edge);
    }
  stencil.translate_axis (ypos_correction, Y_AXIS);
  return stencil;
}

Stencil
vaticana_brew_join (Grob *me, int delta_pitch,
		    Real join_thickness, Real blotdiameter)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  if (!delta_pitch)
    {
      me->programming_error (_ ("Vaticana_ligature: "
				"zero join (delta_pitch == 0)"));
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }
  Interval x_extent = Interval (0, join_thickness);
  Interval y_extent = (delta_pitch > 0)
    ? Interval (0, delta_pitch * 0.5 * staff_space) : // ascending join
    Interval (delta_pitch * 0.5 * staff_space, 0); // descending join
  Box join_box (x_extent, y_extent);
  return Lookup::round_filled_box (join_box, blotdiameter);
}

Stencil
vaticana_brew_primitive (Grob *me)
{
  SCM glyph_name_scm = me->get_property ("glyph-name");
  if (glyph_name_scm == SCM_EOL)
    {
      me->programming_error ("Vaticana_ligature: "
			     "undefined glyph-name -> ignoring grob");
      return Lookup::blank (Box (Interval (0, 0), Interval (0, 0)));
    }

  string glyph_name = ly_scm2string (glyph_name_scm);

  Stencil out;
  Real thickness = robust_scm2double (me->get_property ("thickness"), 1);

  Real line_thickness
    = thickness * me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  Real blotdiameter
    = (me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter")));

  int pos = Staff_symbol_referencer::get_rounded_position (me);

  SCM delta_pitch_scm = me->get_property ("delta-position");
  int delta_pitch;
  if (delta_pitch_scm != SCM_EOL)
    delta_pitch = scm_to_int (delta_pitch_scm);
  else
    delta_pitch = 0;

  Real x_offset = robust_scm2double (me->get_property ("x-offset"), 0);

  bool add_stem = to_boolean (me->get_property ("add-stem"));
  bool add_cauda = to_boolean (me->get_property ("add-cauda"));
  bool add_join = to_boolean (me->get_property ("add-join"));

  if (glyph_name == "")
    {
      /*
       * This is an empty head.  This typically applies for the right
       * side of a curved flexa shape, which is already typeset by the
       * associated left side head.  The only possible thing left to
       * do is to draw a vertical join to the next head.  (Urgh: need
       * flexa_width.)
       */
      Real staff_space = Staff_symbol_referencer::staff_space (me);
      Real flexa_width = robust_scm2double (me->get_property ("flexa-width"), 2) * staff_space;
      out
	= Lookup::blank (Box (Interval (0, 0.5 * flexa_width), Interval (0, 0)));
    }
  else if (glyph_name == "flexa")
    out = vaticana_brew_flexa (me, true, line_thickness);
  else
    {
      out
	= Font_interface::get_default_font (me)->
	find_by_name ("noteheads.s" + glyph_name);
    }
  out.translate_axis (x_offset, X_AXIS);
  Real head_width = out.extent (X_AXIS).length ();

  if (add_cauda)
    {
      Stencil cauda
	= vaticana_brew_cauda (me, pos, delta_pitch,
			       line_thickness, blotdiameter);
      out.add_stencil (cauda);
    }

  if (add_stem)
    {
      Stencil stem
	= vaticana_brew_cauda (me, pos, -1,
			       line_thickness, blotdiameter);
      stem.translate_axis (head_width - line_thickness, X_AXIS);
      out.add_stencil (stem);
    }

  if (add_join)
    {
      Stencil join
	= vaticana_brew_join (me, delta_pitch, line_thickness, blotdiameter);
      join.translate_axis (head_width - line_thickness, X_AXIS);
      out.add_stencil (join);
    }

  return out;
}

MAKE_SCHEME_CALLBACK (Vaticana_ligature, brew_ligature_primitive, 1);
SCM
Vaticana_ligature::brew_ligature_primitive (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM primitive = vaticana_brew_primitive (me).smobbed_copy ();
  return primitive;
}

MAKE_SCHEME_CALLBACK (Vaticana_ligature, print, 1);
SCM
Vaticana_ligature::print (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Vaticana_ligature,
	       "A vaticana style Gregorian ligature.",

	       /* properties */
	       "glyph-name "
	       "flexa-height "
	       "flexa-width "
	       "thickness "
	       "add-cauda "
	       "add-stem "
	       "add-join "
	       "delta-position "
	       "x-offset "
	       );
