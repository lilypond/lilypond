/*
  porrectus.cc -- implement Porrectus

  Copyright (C) 2001 Juergen Reuter

  written for the GNU LilyPond music typesetter

  TODO: --> see porrectus-engraver.cc
*/

#include "staff-symbol-referencer.hh"
#include "porrectus.hh"
#include "item.hh"
#include "molecule.hh"
#include "pitch.hh"
#include "lookup.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "direction.hh"
#include "bezier.hh"
#include "font-interface.hh"
#include "math.h" // rint

void
Porrectus::set_left_head (Grob *me, SCM left_head)
{
  if (left_head == SCM_EOL)
    {
      warning (_ ("(left_head == SCM_EOL) (ignored)"));
    }
  me->set_grob_property ("left-head", left_head);
}

SCM
Porrectus::get_left_head (Grob *me)
{
  SCM left_head = me->get_grob_property ("left-head");
  return left_head;
}

void
Porrectus::set_right_head (Grob *me, SCM right_head)
{
  if (right_head == SCM_EOL)
    {
      warning (_ ("(right_head == SCM_EOL) (ignored)"));
    }
  me->set_grob_property ("right-head", right_head);
}

SCM
Porrectus::get_right_head (Grob *me)
{
  SCM right_head = me->get_grob_property ("right-head");
  return right_head;
}

// Uugh.  The following two functions are almost duplicated code from
// custos.cc, which itself is similar to code in note-head.cc.  Maybe
// this should be moved to staff-symbol-referencer.cc?
Molecule
Porrectus::create_ledger_line (Interval x_extent, Grob *me) 
{
  Molecule line;
  Molecule slice = Font_interface::get_default_font (me)->find_by_name ("noteheads-ledgerending");
  Interval slice_x_extent = slice.extent (X_AXIS);
  Interval slice_y_extent = slice.extent (Y_AXIS);

  // Create left ending of ledger line.
  Molecule left_ending = slice;
  left_ending.translate_axis (x_extent[LEFT] - slice_x_extent[LEFT], X_AXIS);
  if (x_extent.length () > slice_x_extent.length ())
    line.add_molecule (left_ending);

  // Create right ending of ledger line.
  Molecule right_ending = slice;
  right_ending.translate_axis (x_extent[RIGHT] - slice_x_extent[RIGHT],
			       X_AXIS);
  line.add_molecule (right_ending);

  // Fill out space between left and right ending of ledger line by
  // lining up a series of slices in a row between them.
  Molecule fill_out_slice = left_ending;
  Real thick = slice_y_extent.length ();
  Real delta_x = slice_x_extent.length () - thick;
  Real xpos = x_extent [LEFT] + 2*delta_x + thick/2; // TODO: check: thick*2?
  while (xpos <= x_extent[RIGHT])
    {
      fill_out_slice.translate_axis (delta_x, X_AXIS);
      line.add_molecule (fill_out_slice);
      xpos += delta_x;
    }

  return line;
}

Molecule
Porrectus::create_streepjes (Grob *me,
			     int pos,
			     int interspaces,
			     Interval extent)
{
  Real inter_f = Staff_symbol_referencer::staff_space (me)/2;
  int streepjes_i = abs (pos) < interspaces
    ? 0
    : (abs (pos) - interspaces) /2;
  Molecule molecule = Molecule();
  if (streepjes_i) 
    {
      Direction dir = (Direction)sign (pos);
      Molecule ledger_line (create_ledger_line (extent, me));
      ledger_line.set_empty (true);
      Real offs = (Staff_symbol_referencer::on_staffline (me, pos))
	? 0.0
	: -dir * inter_f;
      for (int i = 0; i < streepjes_i; i++)
	{
	  Molecule streep (ledger_line);
	  streep.translate_axis (-dir * inter_f * i * 2 + offs,
				 Y_AXIS);
	  molecule.add_molecule (streep);
	}
    }
  return molecule;
}

MAKE_SCHEME_CALLBACK (Porrectus,brew_molecule,1);
SCM 
Porrectus::brew_molecule (SCM smob)
{
  Item *me = (Item *)unsmob_grob (smob);

  SCM scm_style = me->get_grob_property ("style");
  String style;
  if ((gh_symbol_p (scm_style)) && (scm_style != SCM_EOL))
    style = ly_scm2string (scm_symbol_to_string (scm_style));
  else {
    warning (_ ("porrectus style undefined; using mensural"));
    style = "mensural";
  }

  bool solid = to_boolean (me->get_grob_property ("solid"));
  bool add_stem = to_boolean (me->get_grob_property ("add-stem"));

  SCM stem_direction_scm = me->get_grob_property ("stem-direction");
  Direction stem_direction =
    gh_number_p (stem_direction_scm) ? to_dir (stem_direction_scm) : DOWN;
  if (!stem_direction)
    stem_direction = DOWN;

  SCM left_head_scm = get_left_head (me);
  SCM right_head_scm = get_right_head (me);
  if ((left_head_scm == SCM_EOL) || (right_head_scm == SCM_EOL))
    {
      warning (_ ("junking lonely porrectus"));
      return SCM_EOL;
    }

  Item *left_head = dynamic_cast<Item*> (unsmob_grob (left_head_scm));
  Item *right_head = dynamic_cast<Item*> (unsmob_grob (right_head_scm));
  if (!left_head || !right_head)
    {
      warning (_ ("junking lonely porrectus"));
      return SCM_EOL;
    }

  Real left_position_f = Staff_symbol_referencer::position_f (left_head);
  Real right_position_f = Staff_symbol_referencer::position_f (right_head);
  Real interval = right_position_f - left_position_f;

  Molecule molecule;
  if (String::compare_i (style, "vaticana") == 0)
    molecule = brew_vaticana_molecule (me, solid, add_stem, stem_direction,
				       interval);
  else if (String::compare_i (style, "mensural") == 0)
    molecule = brew_mensural_molecule (me, solid, add_stem, stem_direction,
				       interval);
  else
    return SCM_EOL;

  Real space = Staff_symbol_referencer::staff_space (me);
  Real head_extent = molecule.extent (X_AXIS).length ();
  Interval extent (-0.2 * head_extent, 1.2 * head_extent);
  int interspaces = Staff_symbol_referencer::line_count (me)-1;

  molecule.translate_axis (left_position_f * space/2, Y_AXIS);

  Molecule left_head_streepjes =
    create_streepjes (me, (int)rint (left_position_f), interspaces, extent);
  left_head_streepjes.translate_axis (left_position_f * space/2, Y_AXIS);
  molecule.add_molecule (left_head_streepjes);

  Molecule right_head_streepjes =
    create_streepjes (me, (int)rint (right_position_f), interspaces, extent);
  right_head_streepjes.translate_axis (right_position_f * space/2, Y_AXIS);
  molecule.add_molecule (right_head_streepjes);

  return molecule.smobbed_copy();
}

Molecule
Porrectus::brew_vaticana_molecule (Item *me,
				   bool solid,
				   bool add_stem,
				   Direction stem_direction,
				   Real interval)
{
  Real space = Staff_symbol_referencer::staff_space (me);
  Real line_thickness = space/6;
  Real width = 2.4 * space;
  Molecule molecule = Molecule ();

  if (interval >= 0.0)
    {
      warning (_ ("ascending vaticana style porrectus (ignored)"));
    }

  if (add_stem)
    {
      bool consider_interval =
	((stem_direction == DOWN) && (interval < 0.0)) ||
	((stem_direction == UP) && (interval > 0.0));

      Interval stem_box_x (-line_thickness/2, +line_thickness/2);
      Interval stem_box_y;

      if (consider_interval)
        {
	  Real y_length = interval / 2.0;
	  if (y_length < 1.2 * space)
	    y_length = 1.2 * space;
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, space);

      Real y_correction =
	(stem_direction == UP) ?
	0.3 * space :
	- 0.3 * space - stem_box_y.length();

      Box stem_box (stem_box_x, stem_box_y);
      Molecule stem = Lookup::filledbox (stem_box);
      stem.translate_axis (y_correction, Y_AXIS);
      molecule.add_molecule(stem);
    }

  Box vertical_edge (Interval (-line_thickness/2, +line_thickness/2),
		     Interval (-4*line_thickness/2, +4*line_thickness/2));
  Molecule left_edge = Lookup::filledbox (vertical_edge);
  Molecule right_edge = Lookup::filledbox (vertical_edge);
  right_edge.translate_axis (width, X_AXIS);
  right_edge.translate_axis (interval / 2.0, Y_AXIS);
  molecule.add_molecule(left_edge);
  molecule.add_molecule(right_edge);

  Bezier bezier;
  bezier.control_[0] = Offset (0.00 * width, 0.0);
  bezier.control_[1] = Offset (0.33 * width, interval / 2.0);
  bezier.control_[2] = Offset (0.66 * width, interval / 2.0);
  bezier.control_[3] = Offset (1.00 * width, interval / 2.0);

  Molecule slice;
  slice = Lookup::slur (bezier, 0.0, line_thickness);
  slice.translate_axis (-3 * line_thickness/2, Y_AXIS);
  molecule.add_molecule (slice);
  if (solid)
    for (int i = -2; i < +2; i++)
      {
	slice = Lookup::slur (bezier, 0.0, line_thickness);
	slice.translate_axis (i * line_thickness/2, Y_AXIS);
	molecule.add_molecule (slice);
      }
  slice = Lookup::slur (bezier, 0.0, line_thickness);
  slice.translate_axis (+3 * line_thickness/2, Y_AXIS);
  molecule.add_molecule (slice);

  return molecule;
}

Molecule
Porrectus::brew_mensural_molecule (Item *me,
				   bool solid,
				   bool add_stem,
				   Direction stem_direction,
				   Real interval)
{
  Real space = Staff_symbol_referencer::staff_space (me);
  Real line_thickness = space/6;
  Real width = 2.4 * space;
  Molecule molecule = Molecule ();

  if (add_stem)
    {
      // Uugh.  This is currently the same as in
      // brew_vaticana_molecule, but may eventually be changed.

      bool consider_interval =
	((stem_direction == DOWN) && (interval < 0.0)) ||
	((stem_direction == UP) && (interval > 0.0));

      Interval stem_box_x (0, line_thickness);
      Interval stem_box_y;

      if (consider_interval)
        {
	  Real y_length = interval / 2.0;
	  if (y_length < 1.2 * space)
	    y_length = 1.2 * space;
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, space);

      Real y_correction =
	(stem_direction == UP) ?
	0.3 * space :
	- 0.3 * space - stem_box_y.length();

      Box stem_box (stem_box_x, stem_box_y);
      Molecule stem = Lookup::filledbox (stem_box);
      stem.translate_axis (y_correction, Y_AXIS);
      molecule.add_molecule(stem);
    }

  Real slope = (interval / 2.0) / width;

  Molecule left_edge =
    brew_horizontal_slope (line_thickness, slope, 3.5 * line_thickness);
  left_edge.translate_axis (0.25 * line_thickness, Y_AXIS);
  molecule.add_molecule(left_edge);

  Molecule right_edge =
    brew_horizontal_slope (line_thickness, slope, 3.5 * line_thickness);
  right_edge.translate_axis (width - line_thickness, X_AXIS);
  right_edge.translate_axis (interval / 2.0 * (1.0 - (line_thickness/width)) +
			     0.25 * line_thickness, Y_AXIS);
  molecule.add_molecule(right_edge);

  Molecule bottom_edge =
    Porrectus::brew_horizontal_slope (width, slope, line_thickness);
  bottom_edge.translate_axis (-3 * line_thickness/2, Y_AXIS);
  molecule.add_molecule (bottom_edge);

  Molecule top_edge =
    Porrectus::brew_horizontal_slope (width, slope, line_thickness);
  top_edge.translate_axis (+3 * line_thickness/2, Y_AXIS);
  molecule.add_molecule (top_edge);

  if (solid)
    {
      Molecule core =
	Porrectus::brew_horizontal_slope (width, slope, 6 * line_thickness/2);
      core.translate_axis (-line_thickness/2, Y_AXIS);
      molecule.add_molecule (core);
    }

  return molecule;
}

Molecule
Porrectus::brew_horizontal_slope(Real width, Real slope, Real line_thickness)
{
  SCM width_scm = gh_double2scm (width);
  SCM slope_scm = gh_double2scm (slope);
  SCM line_thickness_scm = gh_double2scm (line_thickness);
  SCM horizontal_slope = gh_list (ly_symbol2scm ("beam"),
				  width_scm, slope_scm,
				  line_thickness_scm, SCM_UNDEFINED);
  Box b (Interval (0, width),
	 Interval (0, width * slope +
		   sqrt (sqr(line_thickness/slope) + sqr (line_thickness))));
  return Molecule (b, horizontal_slope);
}
