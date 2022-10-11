/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

using std::string;
using std::vector;

/*
  TODO: Add support for cubic spline segments.
 */
Stencil
brew_cluster_piece (Grob *me, vector<Offset> bottom_points,
                    vector<Offset> top_points)
{
  Real blotdiameter = Staff_symbol_referencer::staff_space (me) / 2;

  Real padding = from_scm<double> (get_property (me, "padding"), 0.0);

  Offset vpadding = Offset (0, padding);
  Offset hpadding = Offset (0.5 * blotdiameter, 0);
  Offset hvpadding = 0.5 * hpadding + vpadding;

  SCM shape_scm = get_property (me, "style");
  string shape;

  if (scm_is_symbol (shape_scm))
    shape = ly_symbol2string (shape_scm);
  else
    {
      programming_error ("ClusterSpanner.style should be defined as a symbol.");
      me->suicide ();
      return Stencil ();
    }

  Stencil out;
  vector<Offset> points;
  points.clear ();
  vsize size = bottom_points.size ();
  if (size <= 0)
    programming_error ("no points provided");
  else if (shape == "leftsided-stairs")
    {
      for (vsize i = 0; i < size - 1; i++)
        {
          Box box;
          box.add_point (bottom_points[i] - hvpadding);
          box.add_point (
            Offset (top_points[i + 1][X_AXIS], top_points[i][Y_AXIS])
            + hvpadding);
          out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
        }
    }
  else if (shape == "rightsided-stairs")
    {
      for (vsize i = 0; i < size - 1; i++)
        {
          Box box;
          box.add_point (
            Offset (bottom_points[i][X_AXIS], bottom_points[i + 1][Y_AXIS])
            - hvpadding);
          box.add_point (top_points[i + 1] + hvpadding);
          out.add_stencil (Lookup::round_filled_box (box, blotdiameter));
        }
    }
  else if (shape == "centered-stairs")
    {
      Real left_xmid = bottom_points[0][X_AXIS];
      for (vsize i = 0; i < size - 1; i++)
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
      for (vsize i = 1; i < size - 1; i++)
        points.push_back (bottom_points[i] - vpadding);
      points.push_back (bottom_points[size - 1] - vpadding - hpadding);
      points.push_back (top_points[size - 1] + vpadding - hpadding);
      if (size >= 2)
        {
          for (vsize i = size - 2; i > 0; i--)
            points.push_back (top_points[i] + vpadding);
        }
      points.push_back (top_points[0] + vpadding + hpadding);
      out.add_stencil (Lookup::round_polygon (points, blotdiameter, -1.0));
    }
  else
    me->warning (_f ("unknown cluster style `%s'", shape.c_str ()));
  return out;
}

MAKE_SCHEME_CALLBACK (Cluster, calc_cross_staff, "ly:cluster::calc-cross-staff",
                      1);
SCM
Cluster::calc_cross_staff (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Grob, smob, 1);

  extract_grob_set (me, "columns", cols);
  Grob *commony = common_refpoint_of_array (cols, me, Y_AXIS);

  return to_scm (commony != me->get_y_parent ());
}

MAKE_SCHEME_CALLBACK (Cluster, print, "ly:cluster::print", 1);
SCM
Cluster::print (SCM smob)
{
  auto *const me = LY_ASSERT_SMOB (Spanner, smob, 1);

  Item *left_bound = me->get_bound (LEFT);
  Item *right_bound = me->get_bound (RIGHT);

  Grob *commonx = left_bound->common_refpoint (right_bound, X_AXIS);

  vector<Grob *> const &cols = extract_grob_array (me, "columns");
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
  if (Spanner *next = me->broken_neighbor (RIGHT))
    {
      extract_grob_set (next, "columns", next_cols);
      if (next_cols.size () > 0)
        {
          Grob *next_commony
            = common_refpoint_of_array (next_cols, next, Y_AXIS);
          Grob *col = next_cols[0];

          Interval v = col->extent (next_commony, Y_AXIS);
          Real x
            = right_bound->relative_coordinate (commonx, X_AXIS) - left_coord;

          bottom_points.push_back (Offset (x, v[DOWN]));
          top_points.push_back (Offset (x, v[UP]));
        }
    }

  Stencil out = brew_cluster_piece (me, bottom_points, top_points);
  out.translate_axis (-me->relative_coordinate (commony, Y_AXIS), Y_AXIS);
  return out.smobbed_copy ();
}

ADD_INTERFACE (Cluster,
               R"(
A graphically drawn musical cluster.

@code{padding} adds to the vertical extent of the shape (top and bottom).

The property @code{style} controls the shape of cluster segments.  Valid values
include @code{leftsided-stairs}, @code{rightsided-stairs},
@code{centered-stairs}, and @code{ramp}.
               )",

               /* properties */
               R"(
style
padding
columns
               )");

struct Cluster_beacon
{
public:
  DECLARE_SCHEME_CALLBACK (height, (SCM));
};

MAKE_SCHEME_CALLBACK (Cluster_beacon, height, "ly:cluster-beacon::height", 1);
SCM
Cluster_beacon::height (SCM g)
{
  auto *const me = LY_ASSERT_SMOB (Grob, g, 1);
  Interval v = from_scm (get_property (me, "positions"), Interval (0, 0));
  return to_scm (Staff_symbol_referencer::staff_space (me) * 0.5 * v);
}

ADD_INTERFACE (Cluster_beacon,
               R"(
A place holder for the cluster spanner to determine the vertical extents of a
cluster spanner at this X@tie{}position.
               )",

               /* properties */
               R"(
positions
               )");
