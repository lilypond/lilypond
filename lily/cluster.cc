/*
  cluster.cc -- implement Cluster

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Juergen Reuter <reuter@ipd.uka.de>

  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "cluster.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

/*
  TODO: Add support for cubic spline segments.
 */
Stencil
brew_cluster_piece (Grob *me, vector<Offset> bottom_points, vector<Offset> top_points)
{
  Real blotdiameter = Staff_symbol_referencer::staff_space (me) / 2;

  Real padding = robust_scm2double (me->get_property ("padding"), 0.0);

  Offset vpadding = Offset (0, padding);
  Offset hpadding = Offset (0.5 * blotdiameter, 0);
  Offset hvpadding = 0.5 * hpadding + vpadding;

  SCM shape_scm = me->get_property ("style");
  string shape;

  if (scm_is_symbol (shape_scm))
    shape = ly_symbol2string (shape_scm);
  else
    {
      programming_error ("#'style should be symbol.");
      me->suicide ();
      return Stencil ();
    }

  Stencil out;
  vector<Offset> points;
  points.clear ();
  int size = bottom_points.size ();
  if (shape == "leftsided-stairs")
    {
      for (int i = 0; i < size - 1; i++)
	{
	  Box box;
	  box.add_point (bottom_points[i] - hvpadding);
	  box.add_point (Offset (top_points[i + 1][X_AXIS],
				 top_points[i][Y_AXIS]) + hvpadding);
	  out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
	}
    }
  else if (shape == "rightsided-stairs")
    {
      for (int i = 0; i < size - 1; i++)
	{
	  Box box;
	  box.add_point (Offset (bottom_points[i][X_AXIS],
				 bottom_points[i + 1][Y_AXIS]) - hvpadding);
	  box.add_point (top_points[i + 1] + hvpadding);
	  out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
	}
    }
  else if (shape == "centered-stairs")
    {
      Real left_xmid = bottom_points[0][X_AXIS];
      for (int i = 0; i < size - 1; i++)
	{
	  Real right_xmid
	    = 0.5 * (bottom_points[i][X_AXIS] + bottom_points[i + 1][X_AXIS]);
	  Box box;
	  box.add_point (Offset (left_xmid, bottom_points[i][Y_AXIS])
			 - hvpadding);
	  box.add_point (Offset (right_xmid, top_points[i][Y_AXIS])
			 + hvpadding);
	  out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
	  left_xmid = right_xmid;
	}
      Real right_xmid = bottom_points[size - 1][X_AXIS];
      Box box;
      box.add_point (Offset (left_xmid, bottom_points[size - 1][Y_AXIS])
		     - hvpadding);
      box.add_point (Offset (right_xmid, top_points[size - 1][Y_AXIS])
		     + hvpadding);
      out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
    }
  else if (shape == "ramp")
    {
      points.push_back (bottom_points[0] - vpadding + hpadding);
      for (int i = 1; i < size - 1; i++)
	points.push_back (bottom_points[i] - vpadding);
      points.push_back (bottom_points[size - 1] - vpadding - hpadding);
      points.push_back (top_points[size - 1] + vpadding - hpadding);
      for (int i = size - 2; i > 0; i--)
	points.push_back (top_points[i] + vpadding);
      points.push_back (top_points[0] + vpadding + hpadding);
      out.add_stencil (Lookup::round_filled_polygon (points, blotdiameter));
    }
  else
    me->warning (_f ("unknown cluster style `%s'", shape.c_str ()));
  return out;
}

MAKE_SCHEME_CALLBACK (Cluster, calc_cross_staff, 1);
SCM
Cluster::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  extract_grob_set (me, "columns", cols);
  Grob *commony = common_refpoint_of_array (cols, me, Y_AXIS);

  return scm_from_bool (commony != me->get_parent (Y_AXIS));
}

MAKE_SCHEME_CALLBACK (Cluster, print, 1);
SCM
Cluster::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Spanner *spanner = dynamic_cast<Spanner *> (me);
  if (!spanner)
    {
      me->programming_error ("Cluster::print (): not a spanner");
      return SCM_EOL;
    }

  Item *left_bound = spanner->get_bound (LEFT);
  Item *right_bound = spanner->get_bound (RIGHT);

  Grob *commonx = left_bound->common_refpoint (right_bound, X_AXIS);

  vector<Grob*> const &cols = extract_grob_array (me, "columns");
  if (cols.empty ())
    {
      me->warning (_ ("junking empty cluster"));
      me->suicide ();

      return SCM_EOL;
    }

  commonx = common_refpoint_of_array (cols, commonx, X_AXIS);
  Grob *commony = common_refpoint_of_array (cols, me, Y_AXIS);
  vector<Offset> bottom_points;
  vector<Offset> top_points;

  Real left_coord = left_bound->relative_coordinate (commonx, X_AXIS);

  /*
    TODO: should we move the cluster a little to the right to be in
    line with the center of the note heads?

  */
  for (vsize i = 0; i < cols.size (); i++)
    {
      Grob *col = cols[i];

      Interval yext = col->extent (commony, Y_AXIS);

      Real x = col->relative_coordinate (commonx, X_AXIS) - left_coord;
      bottom_points.push_back (Offset (x, yext[DOWN]));
      top_points.push_back (Offset (x, yext[UP]));
    }

  /*
    Across a line break we anticipate on the next pitches.
  */
  if (Spanner *next = spanner->broken_neighbor (RIGHT))
    {
      extract_grob_set (next, "columns", next_cols);
      if (next_cols.size () > 0)
	{
	  Grob *next_commony = common_refpoint_of_array (next_cols, next, Y_AXIS);
	  Grob *col = next_cols[0];

	  Interval v = col->extent (next_commony, Y_AXIS);
	  Real x = right_bound->relative_coordinate (commonx, X_AXIS) - left_coord;

	  bottom_points.push_back (Offset (x, v[DOWN]));
	  top_points.push_back (Offset (x, v[UP]));
	}
    }

  Stencil out = brew_cluster_piece (me, bottom_points, top_points);
  out.translate_axis (- me->relative_coordinate (commony, Y_AXIS), Y_AXIS);
  return out.smobbed_copy ();
}

ADD_INTERFACE (Cluster,
	       "A graphically drawn musical cluster.\n"
	       "\n"
	       "@code{padding} adds to the vertical extent of the shape (top"
	       " and bottom).\n"
	       "\n"
	       "The property @code{style} controls the shape of cluster"
	       " segments.  Valid values include @code{leftsided-stairs},"
	       " @code{rightsided-stairs}, @code{centered-stairs}, and"
	       " @code{ramp}.\n",

	       /* properties */
	       "style "
	       "padding "
	       "columns "
	       );

struct Cluster_beacon
{
public:
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_GROB_INTERFACE ();
};

MAKE_SCHEME_CALLBACK (Cluster_beacon, height, 1);
SCM
Cluster_beacon::height (SCM g)
{
  Grob *me = unsmob_grob (g);
  Interval v = robust_scm2interval (me->get_property ("positions"),
				    Interval (0, 0));
  return ly_interval2scm (Staff_symbol_referencer::staff_space (me) * 0.5 * v);
}

ADD_INTERFACE (Cluster_beacon,
	       "A place holder for the cluster spanner to determine the"
	       " vertical extents of a cluster spanner at this"
	       " X@tie{}position.",

	       /* properties */
	       "positions "
	       );
