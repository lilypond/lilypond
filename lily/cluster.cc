/*
  cluster.cc -- implement Cluster

  source file of the GNU LilyPond music typesetter

  (c) 2002--2003 Juergen Reuter <reuter@ipd.uka.de>

  Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include <stdio.h>

#include "cluster.hh"
#include "spanner.hh"
#include "item.hh"
#include "pitch.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"
#include "box.hh"
#include "interval.hh"
#include "paper-def.hh"
#include "warn.hh"


/*
   TODO: Add support for cubic spline segments.

 */
Molecule
brew_cluster_piece (Grob *me, Array<Offset> bottom_points, Array<Offset> top_points)
{
#if 0
  Real blotdiameter = me->get_paper ()->get_realvar (ly_symbol2scm ("blotdiameter"));
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

  SCM shape_scm = me->get_grob_property ("style");
  String shape;

  if (gh_symbol_p (shape_scm))
    {
      shape = ly_symbol2string (shape_scm);
    }
  else
    {
      programming_error ("#'style should be symbol.");
      me->suicide();
      return  Molecule();
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
      me->warning (_f ("unknown cluster style `%s'", shape.to_str0 ()));
    }
  return out;
}

MAKE_SCHEME_CALLBACK (Cluster,brew_molecule,1);
SCM
Cluster::brew_molecule (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Spanner *spanner = dynamic_cast<Spanner*> (me);
  if (!spanner)
    {
      me->programming_error ("Cluster::brew_molecule(): not a spanner");
      return SCM_EOL;
    }

  Item *left_bound = spanner->get_bound (LEFT);
  Item *right_bound = spanner->get_bound (RIGHT);

  Grob *common = left_bound->common_refpoint (right_bound, X_AXIS);
  SCM cols  =me->get_grob_property ("columns");

  if (!gh_pair_p (cols))
    {
      me->warning ("junking empty cluster");
      me->suicide ();
      
      return SCM_EOL;
    }
  common = common_refpoint_of_list (cols, common, X_AXIS);
  Array<Offset> bottom_points;
  Array<Offset> top_points;


  Real left_coord = left_bound->relative_coordinate (common, X_AXIS);

  Real unit = Staff_symbol_referencer::staff_space (me) *0.5;

  /*
    TODO: should we move the cluster a little to the right to be in
    line with the center of the note heads?
    
   */
  for (SCM s = cols; gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * col = unsmob_grob (ly_car (s));

      SCM posns = col->get_grob_property ("positions");
      
      Slice s (0,0);
      if (ly_number_pair_p (posns))
	s = Slice (gh_scm2int (gh_car (posns)),
		   gh_scm2int (gh_cdr (posns)));

      Real x = col->relative_coordinate (common, X_AXIS) - left_coord;
      bottom_points.push (Offset (x, s[DOWN] *unit));
      top_points.push (Offset (x, s[UP] * unit));
    }

  /*
    Across a line break we anticipate on the next pitches.
   */
  if (spanner->original_)
    {
      Spanner *orig = dynamic_cast<Spanner*> (spanner->original_);
      
      if (spanner->break_index_ < orig->broken_intos_.size()-1)
	{
	  Spanner * next = orig->broken_intos_[spanner->break_index_+1];
	  SCM cols = next->get_grob_property ("columns");
	  if (gh_pair_p (cols))
	    {
	      Grob * col = unsmob_grob (ly_car (scm_last_pair (cols)));
	      SCM posns = col->get_grob_property ("positions");
      
	      Slice s (0,0);
	      if (ly_number_pair_p (posns))
		s = Slice (gh_scm2int (gh_car (posns)),
			   gh_scm2int (gh_cdr (posns)));

	      Real x = right_bound->relative_coordinate (common,X_AXIS) - left_coord;
	      
	      bottom_points.insert (Offset (x, s[DOWN] * unit),0);
	      top_points.insert (Offset (x, s[UP] * unit),0);
	    }
	}
    }

  bottom_points.reverse ();
  top_points.reverse ();

  Molecule out = brew_cluster_piece (me, bottom_points, top_points);
  return out.smobbed_copy ();
}

ADD_INTERFACE (Cluster,"cluster-interface",
  "A graphically drawn musical cluster. " 
"\n\n"
"@code{padding} adds to the vertical extent of the shape (top and "
"bottom) and is expressed in units of staffspace.  Since the pitch "
"range of a single pitch is infinitely small, if padding is set to "
"@code{0.0}, this possibly results in an invisible shape, if you,for "
"example, say @code{c-\\startCluster d e-\\endCluster}.  The default "
"value for @code{padding} therefore is @code{0.25}, such that a single "
"pitch roughly shows the same height as a note head. "
"\n\n"
"@code{style} controls the shape of cluster segments.  Valid values include 'leftsided-stairs', 'rightsided-stairs', 'centered-stairs', and 'ramp'.\n"
,
  "style padding columns");
