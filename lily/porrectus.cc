/*
  porrectus.cc -- implement Porrectus

  Copyright (c) 2001--2002  Juergen Reuter

  written for the GNU LilyPond music typesetter

  TODO: --> see porrectus-engraver.cc
*/

#include "staff-symbol-referencer.hh"
#include "porrectus.hh"
#include "item.hh"
#include "molecule.hh"
#include "pitch.hh"
#include "lookup.hh"
#include "warn.hh"
#include "dimensions.hh"
#include "direction.hh"
#include "bezier.hh"
#include "font-interface.hh"
#include "paper-def.hh"
#include "note-head.hh"
#include "math.h" // rint

void
Porrectus::set_left_head (Grob *me, Item *left_head)
{
  if (left_head != 0)
    {
      me->set_grob_property ("left-head", left_head->self_scm());
    }
  else
    {
      programming_error (_ ("(left_head == 0)"));
      me->set_grob_property ("left-head", SCM_EOL);
    }
}

Item *
Porrectus::get_left_head (Grob *me)
{
  SCM left_head_scm = me->get_grob_property ("left-head");
  if (left_head_scm == SCM_EOL)
    {
      programming_error (_ ("undefined left_head"));
      return 0;
    }
  else
    {
      Item *left_head = unsmob_item (left_head_scm);
      return left_head;
    }
}

void
Porrectus::set_right_head (Grob *me, Item *right_head)
{
  if (right_head != 0)
    {
      me->set_grob_property ("right-head", right_head->self_scm());
    }
  else
    {
      programming_error (_ ("(right_head == 0)"));
      me->set_grob_property ("right-head", SCM_EOL);
    }
}

Item *
Porrectus::get_right_head (Grob *me)
{
  SCM right_head_scm = me->get_grob_property ("right-head");
  if (right_head_scm == SCM_EOL)
    {
      programming_error (_ ("undefined right_head"));
      return 0;
    }
  else
    {
      Item *right_head = unsmob_item (right_head_scm);
      return right_head;
    }
}

MAKE_SCHEME_CALLBACK (Porrectus,brew_molecule,1);
SCM 
Porrectus::brew_molecule (SCM smob)
{
  Item *me = (Item *)unsmob_grob (smob);

  Item *left_head = get_left_head (me);
  Item *right_head = get_right_head (me);
  if (!left_head || !right_head)
    {
      me->warning (_ ("junking lonely porrectus"));
      me->suicide ();
      return SCM_EOL;
    }

  SCM scm_style = me->get_grob_property ("style");
  String style;
  if ((gh_symbol_p (scm_style)) && (scm_style != SCM_EOL))
    style = ly_symbol2string (scm_style);
  else {
    me->warning (_ ("porrectus style undefined; using mensural"));
    style = "mensural";
  }

  bool solid = to_boolean (me->get_grob_property ("solid"));
  bool add_stem = to_boolean (me->get_grob_property ("add-stem"));

  /*
   * This property is called stem-direction (rather than direction)
   * since it only refers to this grob's stem (or, more precisely, its
   * "cauda"), but not the grob as a whole.
   */
  SCM stem_direction_scm = me->get_grob_property ("direction");
  Direction stem_direction =
    gh_number_p (stem_direction_scm) ? to_dir (stem_direction_scm) : DOWN;
  if (!stem_direction)
    stem_direction = DOWN;

  /*
    TODO: revise name.
   */
  bool auto_properties = to_boolean (me->get_grob_property ("auto-properties"));
  if (auto_properties)
      // determine add_stem and stem_direction automatically from durations
    {
      if (String::compare (style, "mensural") != 0)
	me->warning (String("auto-property should be used for\r\n") +
		 String("mensural style porrectus only; trying anyway"));

      int left_duration =
	  gh_scm2int (left_head->get_grob_property ("duration-log"));
      int right_duration =
	  gh_scm2int (right_head->get_grob_property ("duration-log"));

      if ((left_duration == -1) && (right_duration == -1))
        {
	  // brevis -- brevis:
	  // cum proprietate et sine perfectione (c.s.)
	  add_stem = true;
	  stem_direction = DOWN;
	}
      else if ((left_duration == -2) && (right_duration == -1))
        {
	  // longa -- brevis:
	  // sine proprietate et sine perfectione (s.s.)
	  add_stem = false;
	}
      else if ((left_duration == 0) && (right_duration == 0))
        {
	  // semibrevis -- semibrevis:
	  // cum opposita proprietate (c.o.p.)
	  add_stem = true;
	  stem_direction = UP;
	}
      else
        {
	  me->warning (String("auto-property: failed determining porrectus\r\n") +
		   String("properties due to improper durations; ") +
		   String("using user-supplied properties"));
	}
    }

  Real left_position_f = Staff_symbol_referencer::get_position (left_head);
  Real right_position_f = Staff_symbol_referencer::get_position (right_head);
  Real interval = right_position_f - left_position_f;

  Molecule molecule;

  SCM line_thickness_scm = me->get_grob_property ("thickness");
  Real line_thickness;
  if (gh_number_p (line_thickness_scm))
    {
      line_thickness = gh_scm2double (line_thickness_scm);
    }
  else
    {
      line_thickness = 1.0;
    }
  Real thickness =
    line_thickness * me->get_paper ()->get_var ("linethickness");

  SCM porrectus_width_scm = me->get_grob_property ("width");
  Real porrectus_width;
  if (gh_number_p (porrectus_width_scm))
    {
      porrectus_width = gh_scm2double (porrectus_width_scm);
    }
  else
    {
      porrectus_width = 2.4;
    }
  Real width = porrectus_width * Staff_symbol_referencer::staff_space (me);

  if (String::compare (style, "vaticana") == 0)
    molecule = brew_vaticana_molecule (me, interval,
				       solid, width, thickness,
				       add_stem, stem_direction);
  else if (String::compare (style, "mensural") == 0)
    molecule = brew_mensural_molecule (me, interval,
				       solid, width, thickness,
				       add_stem, stem_direction);
  else
    return SCM_EOL;

  Real space = Staff_symbol_referencer::staff_space (me);
  Real head_extent = molecule.extent (X_AXIS).length ();
  Interval extent (-0.2 * head_extent, 1.2 * head_extent);
  int interspaces = Staff_symbol_referencer::line_count (me)-1;

  molecule.translate_axis (left_position_f * space/2, Y_AXIS);

  int left_pos = (int)rint (left_position_f);
  if (abs (left_pos) - interspaces > 1)
    {
      Molecule left_head_ledger_lines =
	Note_head::brew_ledger_lines (me, left_pos, interspaces, extent, true);
      left_head_ledger_lines.translate_axis (left_position_f * space/2,
					     Y_AXIS);
      molecule.add_molecule (left_head_ledger_lines);
    }

  int right_pos = (int)rint (right_position_f);
  if (abs (right_pos) - interspaces > 1)
    {
      Molecule right_head_ledger_lines =
	Note_head::brew_ledger_lines (me, right_pos, interspaces, extent, true);
      right_head_ledger_lines.translate_axis (right_position_f * space/2,
					      Y_AXIS);
      molecule.add_molecule (right_head_ledger_lines);
    }

  return molecule.smobbed_copy();
}

Molecule
Porrectus::brew_vaticana_molecule (Item *me,
				   Real interval,
				   bool solid,
				   Real width,
				   Real thickness,
				   bool add_stem,
				   Direction stem_direction)
{
  if (interval >= 0.0)
    {
      me->warning (_ ("ascending vaticana style porrectus"));
    }

  Real space = Staff_symbol_referencer::staff_space (me);
  Molecule molecule = Molecule ();
  Real right_height = 0.6 * space;

  // Compensate thickness that appears to be smaller in steep section
  // of bend.
  Real left_height = right_height + min (0.12 * abs(interval), 0.3) * space;

  if (add_stem)
    {
      bool consider_interval =
	stem_direction * interval > 0.0;

      Interval stem_box_x (0, thickness);
      Interval stem_box_y;

      if (consider_interval)
	{
	  Real y_length = max (abs(interval)/2.0*space +
			       (right_height-left_height),
			       1.2*space);
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, space);

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
  Real ypos_correction = -0.1*space * sign(interval);
  Real interval_correction = 0.2*space * sign(interval);
  Real corrected_interval = interval*space + interval_correction;

  // middle curve of vaticana style porrectus
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
      // out from the porrectus shape if the interval is big and the
      // line thickness small.  The difficulty here is to compute a
      // proper slope value, as it should roughly be equal with the
      // slope of the left end of the bezier curve.
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

Molecule
Porrectus::brew_mensural_molecule (Item *me,
				   Real interval,
				   bool solid,
				   Real width,
				   Real thickness,
				   bool add_stem,
				   Direction stem_direction)
{
  Real space = Staff_symbol_referencer::staff_space (me);
  Real height = 0.6 * space;
  Molecule molecule = Molecule ();

  if (add_stem)
    {
      bool consider_interval =
	stem_direction * interval > 0.0;

      Interval stem_box_x (0, thickness);
      Interval stem_box_y;

      if (consider_interval)
        {
	  Real y_length = max (interval/2.0*space, 1.2*space);
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, space);

      Real y_correction =
	(stem_direction == UP) ?
	+0.5*height :
	-0.5*height - stem_box_y.length();

      Box stem_box (stem_box_x, stem_box_y);
      Molecule stem = Lookup::filledbox (stem_box);
      stem.translate_axis (y_correction, Y_AXIS);
      molecule.add_molecule(stem);
    }

  Real slope = (interval / 2.0 * space) / width;

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to slope.
  Real ypos_correction = -0.1*space * sign(slope);
  Real slope_correction = 0.2*space * sign(slope);
  Real corrected_slope = slope + slope_correction/width;

  if (solid)
    {
      Molecule solid_head =
	Lookup::horizontal_slope (width, corrected_slope, height);
      molecule.add_molecule (solid_head);
    }
  else // outline
    {
      Molecule left_edge =
	Lookup::horizontal_slope (thickness, corrected_slope, height);
      molecule.add_molecule(left_edge);

      Molecule right_edge =
	Lookup::horizontal_slope (thickness, corrected_slope, height);
      right_edge.translate_axis (width-thickness, X_AXIS);
      right_edge.translate_axis (corrected_slope * (width-thickness), Y_AXIS);
      molecule.add_molecule(right_edge);

      Molecule bottom_edge =
	Lookup::horizontal_slope (width, corrected_slope, thickness);
      bottom_edge.translate_axis (-0.5*height, Y_AXIS);
      molecule.add_molecule (bottom_edge);

      Molecule top_edge =
	Lookup::horizontal_slope (width, corrected_slope, thickness);
      top_edge.translate_axis (+0.5*height, Y_AXIS);
      molecule.add_molecule (top_edge);
    }
  molecule.translate_axis (ypos_correction, Y_AXIS);
  return molecule;
}


ADD_INTERFACE (Porrectus,"porrectus-interface",
  "A porrectus ligature, joining two note heads into a single grob.",
  "left-head right-head width add-stem auto-properties solid direction");
