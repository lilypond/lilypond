/*
  cluster.cc -- implement Cluster

  source file of the GNU LilyPond music typesetter

  (c) 2002 Juergen Reuter <reuter@ipd.uka.de>
*/

#include <stdio.h>
#include "cluster.hh"
#include "grob.hh"
#include "spanner.hh"
#include "item.hh"
#include "pitch.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "box.hh"
#include "interval.hh"
#include "paper-def.hh"
#include "paper-column.hh"

/*
 * TODO: Add support for cubic spline segments.
 */
Molecule
brew_cluster_piece (Grob *me, Array<Offset> bottom_points, Array<Offset> top_points)
{
#if 0
  Real blotdiameter = me->get_paper ()->get_var ("blotdiameter");
#else
  Real blotdiameter = Staff_symbol_referencer::staff_space (me)/2;
#endif

  Real padding;
  SCM padding_scm = me->get_grob_property ("padding");
  if (gh_number_p (padding_scm))
    padding = gh_scm2double (padding_scm);
  else
    padding = 0.0;
  Offset vpadding = Offset (0, padding);
  Offset hpadding = Offset (0.5 * blotdiameter, 0);
  Offset hvpadding = 0.5 * hpadding + vpadding;

  SCM shape_scm = me->get_grob_property ("shape");
  String shape;
  if (gh_symbol_p (shape_scm))
    {
      shape = ly_symbol2string (shape_scm);
    }
  else
    {
      shape = "leftsided-stairs";
    }


  Molecule out = Molecule ();
  Array<Offset> points;
  points.clear ();
  int size = bottom_points.size ();
  if (String::compare (shape, "leftsided-stairs") == 0)
    {
      for (int i = 0; i < size - 1; i++)
	{
	  Box box;
	  box.add_point (bottom_points[i] - hvpadding);
	  box.add_point (Offset(top_points[i + 1][X_AXIS],
				top_points[i][Y_AXIS]) + hvpadding);
	  out.add_molecule (Lookup::roundfilledbox (box, blotdiameter));
	}
    }
  else if (String::compare (shape, "rightsided-stairs") == 0)
    {
      for (int i = 0; i < size - 1; i++)
	{
	  Box box;
	  box.add_point (Offset(bottom_points[i][X_AXIS],
				bottom_points[i + 1][Y_AXIS]) - hvpadding);
	  box.add_point (top_points[i + 1] + hvpadding);
	  out.add_molecule (Lookup::roundfilledbox (box, blotdiameter));
	}
    }
  else if (String::compare (shape, "centered-stairs") == 0)
    {
      Real left_xmid = bottom_points[0][X_AXIS];
      for (int i = 0; i < size - 1; i++)
	{
	  Real right_xmid =
	    0.5 * (bottom_points[i][X_AXIS] + bottom_points[i + 1][X_AXIS]);
	  Box box;
	  box.add_point (Offset (left_xmid, bottom_points[i][Y_AXIS]) -
			 hvpadding);
	  box.add_point (Offset (right_xmid, top_points[i][Y_AXIS]) +
			 hvpadding);
	  out.add_molecule (Lookup::roundfilledbox (box, blotdiameter));
	  left_xmid = right_xmid;
	}
      Real right_xmid = bottom_points[size - 1][X_AXIS];
      Box box;
      box.add_point (Offset (left_xmid, bottom_points[size - 1][Y_AXIS]) -
		     hvpadding);
      box.add_point (Offset (right_xmid, top_points[size - 1][Y_AXIS]) +
		     hvpadding);
      out.add_molecule (Lookup::roundfilledbox (box, blotdiameter));
    }
  else if (String::compare (shape, "ramp") == 0)
    {
      points.push (bottom_points[0] - vpadding + hpadding);
      for (int i = 1; i < size - 1; i++)
	{
	  points.push (bottom_points[i] - vpadding);
	}
      points.push (bottom_points[size - 1] - vpadding - hpadding);
      points.push (top_points[size - 1] + vpadding - hpadding);
      for (int i = size - 2; i > 0; i--)
	{
	  points.push (top_points[i] + vpadding);
	}
      points.push (top_points[0] + vpadding + hpadding);
      out.add_molecule (Lookup::round_filled_polygon (points, blotdiameter));
    }
  else
    {
      me->warning (_f ("unknown cluster shape `%s'", shape.to_str0 ()));
    }
  return out;
}

MAKE_SCHEME_CALLBACK (Cluster,brew_molecule,1);
SCM
Cluster::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Spanner *spanner = dynamic_cast<Spanner*> (me);
  if (!spanner) {
    me->programming_error ("Cluster::brew_molecule(): not a spanner");
    return SCM_EOL;
  }

  Item *left_bound = spanner->get_bound (LEFT);
  Item *right_bound = spanner->get_bound (RIGHT);
  bool right_broken = right_bound->break_status_dir () != CENTER;

  Grob *common = left_bound->common_refpoint (right_bound, X_AXIS);

  Grob *column = 0;
  Array<Offset> bottom_points;
  Array<Offset> top_points;
  bottom_points.clear ();
  top_points.clear ();
  SCM column_scm = SCM_EOL;
  for (SCM columns_scm = me->get_grob_property ("segments");
       columns_scm != SCM_EOL;
       columns_scm = ly_cdr (columns_scm)) {
    column_scm = ly_car (columns_scm);
    SCM col_scm = ly_car (column_scm);
    if (gh_number_p (col_scm))
      // broken spanner: this column not in this piece
      if (!column)
	continue; // still have to expect columns
      else
	break; // ok, we have seen all columns
    column = unsmob_grob (col_scm);
    column_scm = ly_cdr (column_scm);
    Real y = 0.5 * gh_scm2double (ly_car (column_scm));
    column_scm = ly_cdr (column_scm);
    Pitch *pitch_min = unsmob_pitch (ly_car (column_scm));
    column_scm = ly_cdr (column_scm);
    Pitch *pitch_max = unsmob_pitch (ly_car (column_scm));
    Real height = 0.5 * (pitch_max->steps () - pitch_min->steps ());
    Real x = column->relative_coordinate (common, X_AXIS);
    if (right_broken)
      x -= left_bound->relative_coordinate (common, X_AXIS);
    bottom_points.push (Offset (x, y));
    top_points.push (Offset (x, y + height));
  }
  if (right_broken)
    {
      Real y = 0.5 * gh_scm2double (ly_car (column_scm));
      column_scm = ly_cdr (column_scm);
      Pitch *pitch_min = unsmob_pitch (ly_car (column_scm));
      column_scm = ly_cdr (column_scm);
      Pitch *pitch_max = unsmob_pitch (ly_car (column_scm));
      Real height = 0.5 * (pitch_max->steps () - pitch_min->steps ());
      Real x =
	right_bound->relative_coordinate (common, X_AXIS) -
	left_bound->relative_coordinate (common, X_AXIS);
      bottom_points.push (Offset (x, y));
      top_points.push (Offset (x, y + height));
    }
  Molecule out = brew_cluster_piece (me, bottom_points, top_points);
  return out.smobbed_copy ();
}

ADD_INTERFACE (Cluster,"cluster-interface",
  "A graphically drawn musical cluster.",
  "shape padding");
