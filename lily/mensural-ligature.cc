/*
  mensural-ligature.cc -- implement Mensural_ligature
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include <math.h>
#include "item.hh"
#include "mensural-ligature.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "note-head.hh"
#include "paper-def.hh"
#include "warn.hh"

/*
 * TODO: divide this function into mensural and neo-mensural style.
 *
 * TODO: move this function to class Lookup?
 */
Molecule
brew_flexa (Grob *me,
	    Real interval,
	    bool solid,
	    Real width,
	    Real thickness,
	    bool add_stem,
	    Direction stem_direction)
{
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real height = 0.6 * staff_space;
  Molecule molecule = Molecule ();

  if (add_stem)
    {
      bool consider_interval =
	stem_direction * interval > 0.0;

      Interval stem_box_x (0, thickness);
      Interval stem_box_y;

      if (consider_interval)
        {
	  Real y_length = max (interval/2.0*staff_space, 1.2*staff_space);
	  stem_box_y = Interval (0, y_length);
	}
      else
	stem_box_y = Interval (0, staff_space);

      Real y_correction =
	(stem_direction == UP) ?
	+0.5*height :
	-0.5*height - stem_box_y.length();

      Box stem_box (stem_box_x, stem_box_y);
      Molecule stem = Lookup::filledbox (stem_box);
      stem.translate_axis (y_correction, Y_AXIS);
      molecule.add_molecule(stem);
    }

  Real slope = (interval / 2.0 * staff_space) / width;

  // Compensate optical illusion regarding vertical position of left
  // and right endings due to slope.
  Real ypos_correction = -0.1*staff_space * sign(slope);
  Real slope_correction = 0.2*staff_space * sign(slope);
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

void
add_ledger_lines (Grob *me, Molecule *out, int pos, Real offs,
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
internal_brew_primitive (Grob *me, bool ledger_take_space)
{
  SCM primitive_scm = me->get_grob_property ("primitive");
  if (primitive_scm == SCM_EOL)
    {
      programming_error ("Mensural_ligature: undefined primitive -> ignoring grob");
      return Molecule ();
    }

  Molecule out;
  int primitive = gh_scm2int (primitive_scm);
  int delta_pitch = 0;
  Real thickness = 0.0;
  Real flexa_width = 0.0;
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  if (primitive & MLP_ANY)
    {
      SCM thickness_scm = me->get_grob_property ("thickness");
      if (thickness_scm != SCM_EOL)
	{
	  thickness = gh_scm2double (thickness_scm);
	}
      else
	{
	  programming_error (_f ("Mensural_ligature: thickness undefined on flexa %d; assuming 1.4", primitive));
	  thickness = 1.4 * me->paper_l ()->get_var ("linethickness");
	}
    }

  if (primitive & MLP_FLEXA)
    {
      SCM delta_pitch_scm = me->get_grob_property ("delta-pitch");
      if (delta_pitch_scm != SCM_EOL)
	{
	  delta_pitch = gh_scm2int (delta_pitch_scm);
	}
      else
	{
	  programming_error (_f ("Mensural_ligature: delta-pitch undefined on flexa %d; assuming 0", primitive));
	  delta_pitch = 0;
	}

      SCM flexa_width_scm = me->get_grob_property ("flexa-width");
      if (flexa_width_scm != SCM_EOL)
	{
	  flexa_width = gh_scm2double (flexa_width_scm);
	}
      else
	{
	  programming_error (_f ("Mensural_ligature: flexa-width undefined on flexa %d; assuming 2.0", primitive));
	  flexa_width = 2.0 * staff_space;
	}
    }

  switch (primitive)
    {
      case MLP_NONE:
	return Molecule();
      case MLP_BB:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, true, DOWN);
	break;
      case MLP_sc:
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads--2mensural");
	break;
      case MLP_ss:
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads--1mensural");
	break;
      case MLP_cs:
	out = Font_interface::get_default_font (me)->find_by_name ("noteheads-lmensural");
	break;
      case MLP_SS:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, true, UP);
	break;
      case MLP_LB:
	out = brew_flexa (me, delta_pitch, false,
			  flexa_width, thickness, false, CENTER);
	break;
      default:
	programming_error (_f ("Mensural_ligature: unexpected case fall-through"));
	return Molecule ();
    }

  SCM join_left_scm = me->get_grob_property ("join-left");
  if (join_left_scm != SCM_EOL)
    {
      int join_left = gh_scm2int (join_left_scm);
      if (!join_left)
	programming_error (_f ("Menusral_ligature: (join_left == 0)"));
      Real blotdiameter = (me->paper_l ()->get_var ("blotdiameter"));
      Interval x_extent = Interval (0, thickness);
      Interval y_extent = (join_left > 0) ?
	Interval (-join_left * 0.5 * staff_space, 0) :
	Interval (0, -join_left * 0.5 * staff_space);
      Box stem_box (x_extent, y_extent);

      Molecule stem =
	Lookup::roundfilledbox (stem_box, blotdiameter);
      out.add_molecule (stem);
    }

  int pos = (int)rint (Staff_symbol_referencer::position_f (me));
  add_ledger_lines(me, &out, pos, 0, ledger_take_space);
  if (primitive & MLP_FLEXA)
    {
      pos += delta_pitch;
      add_ledger_lines(me, &out, pos, 0.5*delta_pitch, ledger_take_space);
    }

  return out;
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, brew_ligature_primitive, 1);
SCM
Mensural_ligature::brew_ligature_primitive (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return internal_brew_primitive (me, false).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Mensural_ligature, brew_molecule, 1);
SCM
Mensural_ligature::brew_molecule (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE(Mensural_ligature, "mensural-ligature-interface",
	      "A mensural ligature",
	      "thickness flexa-width");
