/*
  vaticana-ligature.cc -- implement Vaticana_ligature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2003 Juergen Reuter <reuter@ipd.uka.de>
*/

#include <math.h>
#include "item.hh"
#include "vaticana-ligature.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "paper-def.hh"
#include "bezier.hh"
#include "warn.hh"

/*
 * TODO: move this function to class Lookup?
 */
Molecule
vaticana_brew_flexa (Grob *me,
		     bool solid,
		     Real thickness,
		     Direction stem_direction)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Molecule molecule = Molecule ();
  Real right_height = 0.6 * staff_space;

  Real interval;
  SCM flexa_height_scm = me->get_grob_property ("flexa-height");
  if (flexa_height_scm != SCM_EOL)
    {
      interval = gh_scm2int (flexa_height_scm);
    }
  else
    {
      me->warning ("Vaticana_ligature: "
		   "flexa-height undefined; assuming 0");
      interval = 0.0;
    }

  if (interval >= 0.0)
    {
      me->warning (_ ("ascending vaticana style flexa"));
    }

  Real width;
  SCM flexa_width_scm = me->get_grob_property ("flexa-width");
  if (flexa_width_scm != SCM_EOL)
    {
      width = gh_scm2double (flexa_width_scm);
    }
  else
    {
      me->warning ("Vaticana_ligature:"
		   "flexa-width undefined; assuming 2.0");
      width = 2.0 * staff_space;
    }

  bool add_stem = to_boolean (me->get_grob_property ("add-stem"));

  // Compensate thickness that appears to be smaller in steep section
  // of bend.
  Real left_height =
    right_height +
    min (0.12 * abs(interval), 0.3) * staff_space;

  if (add_stem)
    {
      bool consider_interval =
	stem_direction * interval > 0.0;

      Interval stem_box_x (0, thickness);
      Interval stem_box_y;

      if (consider_interval)
	{
	  Real y_length = max (abs(interval)/2.0*staff_space +
			       (right_height-left_height),
			       1.2*staff_space);
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, staff_space);

      Real y_correction =
	(stem_direction == UP) ?
	+0.5*left_height :
	-0.5*left_height - stem_box_y.length();

      Box stem_box (stem_box_x, stem_box_y);
      Molecule stem = Lookup::filledbox (stem_box);
      stem.translate_axis (y_correction, Y_AXIS);
      molecule.add_molecule(stem);
    }

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to curved shape.
  Real ypos_correction = -0.1*staff_space * sign(interval);
  Real interval_correction = 0.2*staff_space * sign(interval);
  Real corrected_interval = interval*staff_space + interval_correction;

  // middle curve of flexa shape
  Bezier curve;
  curve.control_[0] = Offset (0.00 * width, 0.0);
  curve.control_[1] = Offset (0.33 * width, corrected_interval / 2.0);
  curve.control_[2] = Offset (0.66 * width, corrected_interval / 2.0);
  curve.control_[3] = Offset (1.00 * width, corrected_interval / 2.0);

  Bezier top_curve = curve, bottom_curve = curve;
  for (int i = 0; i < 4; i++)
    {
      Real thickness = 0.33 * ((3 - i)*left_height + i*right_height);
      top_curve.control_[i] += Offset (0, +0.5*thickness);
      bottom_curve.control_[i] += Offset (0, -0.5*thickness);
    }

  if (solid)
    {
      Molecule solid_head =
	Lookup::bezier_sandwich (top_curve, bottom_curve);
      molecule.add_molecule (solid_head);
    }
  else // outline
    {
      Bezier inner_top_curve = top_curve;
      inner_top_curve.translate (Offset (0.0, -thickness));
      Molecule top_edge =
	Lookup::bezier_sandwich (top_curve, inner_top_curve);
      molecule.add_molecule(top_edge);

      Bezier inner_bottom_curve = bottom_curve;
      inner_bottom_curve.translate (Offset (0.0, +thickness));
      Molecule bottom_edge =
	Lookup::bezier_sandwich (bottom_curve, inner_bottom_curve);
      molecule.add_molecule(bottom_edge);

      // TODO: Use horizontal slope with proper slope value rather
      // than filled box for left edge, since the filled box stands
      // out from the flexa shape if the interval is big and the line
      // thickness small.  The difficulty here is to compute a proper
      // slope value, as it should roughly be equal with the slope of
      // the left end of the bezier curve.
      Box left_edge_box (Interval (0, thickness),
			 Interval (-0.5*left_height, +0.5*left_height));
      Molecule left_edge = Lookup::filledbox (left_edge_box);
      molecule.add_molecule(left_edge);

      Box right_edge_box (Interval (-thickness, 0),
			  Interval (-0.5*right_height, +0.5*right_height));
      Molecule right_edge = Lookup::filledbox (right_edge_box);
      right_edge.translate_axis (width, X_AXIS);
      right_edge.translate_axis (corrected_interval / 2.0, Y_AXIS);
      molecule.add_molecule(right_edge);
    }
  molecule.translate_axis (ypos_correction, Y_AXIS);
  return molecule;
}

void
vaticana_add_ledger_lines (Grob *me, Molecule *out, int pos, Real offs,
			   bool ledger_take_space)
{
  int interspaces = Staff_symbol_referencer::line_count (me)-1;
  if (abs (pos) - interspaces > 1)
    {
      Interval hd = out->extent (X_AXIS);
      Real left_ledger_protusion = hd.length ()/4;
      Real right_ledger_protusion = left_ledger_protusion;

      Interval l_extents = Interval (hd[LEFT] - left_ledger_protusion,
				     hd[RIGHT] + right_ledger_protusion);
      Molecule ledger_lines =
	Note_head::brew_ledger_lines (me, pos, interspaces,
				      l_extents,
				      ledger_take_space);
      ledger_lines.translate_axis (offs, Y_AXIS);
      out->add_molecule (ledger_lines);
    }
}

Molecule
vaticana_brew_primitive (Grob *me, bool ledger_take_space)
{
  SCM glyph_name_scm = me->get_grob_property ("glyph-name");
  if (glyph_name_scm == SCM_EOL)
    {
      programming_error ("Vaticana_ligature:"
			 "undefined glyph-name -> ignoring grob");
      return Molecule ();
    }

  String glyph_name = ly_scm2string (glyph_name_scm);
  if (!String::compare (glyph_name, ""))
    {
      // empty head (typically, this is the right side of flexa shape,
      // which is already typeset by the associated left side head);
      // nothing left to do
      return Molecule ();
    }

  Molecule out;
  int flexa_height = 0;
  Real thickness = 0.0;
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  SCM thickness_scm = me->get_grob_property ("thickness");
  if (thickness_scm != SCM_EOL)
    {
      thickness = gh_scm2double (thickness_scm);
    }
  else
    {
      programming_error (_f ("Vaticana_ligature:"
			     "thickness undefined; assuming 1.4",
			     me));
      thickness = 1.4 * me->get_paper ()->get_var ("linethickness");
    }

  Real x_offset = 0.0;
  SCM x_offset_scm = me->get_grob_property ("x-offset");
  if (x_offset_scm != SCM_EOL)
    {
      x_offset = gh_scm2double (x_offset_scm);
    }
  else
    {
      programming_error (_f ("Vaticana_ligature:"
			     "x-offset undefined; assuming 0.0",
			     me));
    }

  if (!String::compare (glyph_name, "flexa"))
    {
      out = vaticana_brew_flexa (me, true, thickness, DOWN);
    }
  else
    {
      Molecule mol =
	Font_interface::get_default_font (me)->
	find_by_name ("noteheads-" + glyph_name);
      mol.translate_axis (x_offset, X_AXIS);
      out.add_molecule (mol);
    }

  SCM join_left_scm = me->get_grob_property ("join-left");
  if (join_left_scm != SCM_EOL)
    {
      int join_left = gh_scm2int (join_left_scm);
      if (!join_left)
	programming_error (_f ("Vaticana_ligature: (join_left == 0)"));
      Real blotdiameter = (me->get_paper ()->get_var ("blotdiameter"));
      Interval x_extent =
	Interval (-0.5 * thickness, +0.5 * thickness);
      Interval y_extent = (join_left > 0) ?
	Interval (-join_left * 0.5 * staff_space, 0) : // ascending join
	Interval (0, -join_left * 0.5 * staff_space); // descending join
      Box stem_box (x_extent, y_extent);

      Molecule stem = Lookup::roundfilledbox (stem_box, blotdiameter);
      out.add_molecule (stem);
    }

  int pos = (int)rint (Staff_symbol_referencer::get_position (me));
  vaticana_add_ledger_lines(me, &out, pos, 0, ledger_take_space);
  if (!String::compare (glyph_name, "flexa"))
    {
      pos += flexa_height;
      vaticana_add_ledger_lines(me, &out, pos, 0.5*flexa_height, ledger_take_space);
    }

  return out;
}

MAKE_SCHEME_CALLBACK (Vaticana_ligature, brew_ligature_primitive, 1);
SCM
Vaticana_ligature::brew_ligature_primitive (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM primitive = vaticana_brew_primitive (me, false).smobbed_copy ();
  return primitive;
}

MAKE_SCHEME_CALLBACK (Vaticana_ligature, brew_molecule, 1);
SCM
Vaticana_ligature::brew_molecule (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Vaticana_ligature, "vaticana-ligature-interface",
	       "A vaticana style gregorian ligature",
	       "glyph-name flexa-height flexa-width thickness join-left "
	       "add-stem x-offset ligature-primitive-callback");
