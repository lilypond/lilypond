/*
  ligature-bracket.cc -- implement Ligature_bracket
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "ligature-bracket.hh"
#include "item.hh"
#include "paper-def.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "box.hh"

static Molecule
brew_edge (Direction dir, Real thickness, Real width, Real height,
	   Real blotdiameter)
{
  Molecule hline = Lookup::roundfilledbox (Box (Interval (0, width),
						Interval (0, thickness)),
					   blotdiameter);
  hline.translate_axis (height - thickness, Y_AXIS);

  Molecule vline = Lookup::roundfilledbox (Box (Interval (0, thickness),
						Interval (0, height)),
					   blotdiameter);
  if (dir == RIGHT)
    {
      vline.translate_axis (width - thickness, X_AXIS);
    }

  Molecule edge = Molecule ();
  edge.add_molecule (hline);
  edge.add_molecule (vline);
  return edge;
}

MAKE_SCHEME_CALLBACK (Ligature_bracket, brew_molecule, 1);
SCM
Ligature_bracket::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner*> (me);
  Real blotdiameter = me->get_paper ()->get_var ("blotdiameter");
  Real staff_space = Staff_symbol_referencer::staff_space (me);

  Real thickness = me->get_paper ()->get_var ("linethickness");  
  SCM grob_thickness = me->get_grob_property ("thickness");
  if (gh_number_p (grob_thickness))
    thickness *= gh_scm2double (grob_thickness);

  SCM edge_width_scm = me->get_grob_property ("width");
  Real edge_width;
  if (gh_number_p (edge_width_scm))
    {
      edge_width = gh_scm2double (edge_width_scm);
    }
  else
    {
      edge_width = 0.75;
    }
  edge_width *= staff_space;

  SCM edge_height_scm = me->get_grob_property ("height");
  Real edge_height;
  if (gh_number_p (edge_height_scm))
    {
      edge_height = gh_scm2double (edge_height_scm);
    }
  else
    {
      edge_height = 0.5;
    }
  edge_height *= staff_space;

  Item* left_bound = spanner->get_bound (LEFT);
  Item* right_bound = spanner->get_bound (RIGHT);

  Molecule bracket = Molecule ();

  Real y_min_offs =
    0.5 * Staff_symbol_referencer::line_count (me) * staff_space;
  Real y_left_offs = y_min_offs;
  Real y_right_offs = y_min_offs;

  Real left_bound_left_extent = 0;

  if (left_bound)
    {
      Molecule left_edge =
	brew_edge (LEFT, thickness, edge_width, edge_height, blotdiameter);
      Grob *left_common_x = me->common_refpoint (left_bound, X_AXIS);
      left_bound_left_extent =
	left_bound->extent (left_common_x, X_AXIS)[LEFT];
      left_edge.translate_axis (left_bound_left_extent, X_AXIS);
      bracket.add_molecule (left_edge);
      Grob *left_common_y = me->common_refpoint (left_bound, Y_AXIS);
      y_left_offs =
	max(y_left_offs, left_bound->extent (left_common_y, Y_AXIS)[UP]);
    }
  else
    {
      me->warning (_ ("no left bound"));
    }

  if (right_bound)
    {
      Molecule right_edge =
	brew_edge (RIGHT, thickness, edge_width, edge_height, blotdiameter);
      Grob *staff_symbol = Staff_symbol_referencer::get_staff_symbol (me);
      Grob *right_common_bound_x =
	right_bound->common_refpoint (staff_symbol, X_AXIS);

      Real left_offs = 0;
      if (left_bound)
	{
	  Grob *left_common_bound_x =
	    left_bound->common_refpoint (staff_symbol, X_AXIS);
	  left_offs = left_bound->extent (left_common_bound_x, X_AXIS)[LEFT];
	}

      Real right_offs =
	right_bound->extent (right_common_bound_x, X_AXIS)[RIGHT];

      right_edge.translate_axis (+ right_offs
				 - left_offs
				 + left_bound_left_extent
				 - edge_width,
				 X_AXIS);
      bracket.add_molecule (right_edge);
      Grob *right_common_y = me->common_refpoint (right_bound, Y_AXIS);
      y_right_offs =
	max(y_right_offs, right_bound->extent (right_common_y, Y_AXIS)[UP]);
    }
  else
    {
      me->warning (_ ("no left bound"));
    }

  bracket.translate_axis (max (y_left_offs, y_right_offs), Y_AXIS);

  return bracket.smobbed_copy ();
}

ADD_INTERFACE(Ligature_bracket, "ligature-bracket-interface",
	      "A bracket indicating a ligature in the original edition",
	      "width thickness height");
