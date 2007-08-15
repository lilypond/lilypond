/*
  skyline.cc -- implement Skyline_entry and funcs.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "skyline.hh"

/*
  A skyline is a shape of the form:


  *                  ----
  *                  |  |
  *         ---------|  |
  *         |           |
  *         |           |
  *         |	        |______
  * --------|		      |___
  *

  This file deals with building such skyline structure, and computing
  the minimum distance between two opposing skylines.

  Invariants for a skyline:

  skyline[...].width_ forms a partition of the real interval, where
  the segments are adjacent, and ascending. Hence we have

  skyline.back ().width_[RIGHT] = inf
  skyline[0].width_[LEFT] = -inf
*/

const Real EPS = 1e-12;

/*
  TODO: avoid unnecessary fragmentation.

  This is O (n^2), searching and insertion.  Could be O (n log n) with
  binsearch.
*/
void
insert_extent_into_skyline (vector<Skyline_entry> *line, Box b, Axis line_axis,
			    Direction d)
{
  Interval extent = b[line_axis];
  if (extent.is_empty ())
    return;

  Real stick_out = b[other_axis (line_axis)][d];

  /*
    Intersect each segment of LINE with EXTENT, and if non-empty, insert relevant segments.
  */
  for (vsize i = line->size (); i--;)
    {
      Interval w = line->at (i).width_;
      w.intersect (extent);

      if (extent[LEFT] >= w[RIGHT])
	break;

      Real my_height = line->at (i).height_;

      if (!w.is_empty ()
	  && w.length () > EPS
	  && d * (my_height - stick_out) < 0)
	{
	  Interval e1 (line->at (i).width_[LEFT], extent[LEFT]);
	  Interval e3 (extent[RIGHT], line->at (i).width_[RIGHT]);

	  if (!e3.is_empty () && e3.length () > EPS)
	    line->insert (line->begin () + i + 1, Skyline_entry (e3, my_height));

	  line->at (i).height_ = stick_out;
	  line->at (i).width_ = w;
	  if (!e1.is_empty () && e1.length () > EPS)
	    line->insert (line->begin () + i, Skyline_entry (e1, my_height));
	}
    }
}

void
merge_skyline (vector<Skyline_entry> *a1,
	       vector<Skyline_entry> const &a2,
	       Direction dir)
{
  for (vsize i = 0; i < a2.size (); i++)
    {
      Box b;
      b[X_AXIS] = a2[i].width_;
      b[Y_AXIS][dir] = a2[i].height_;
      b[Y_AXIS][-dir] = dir * infinity_f;

      insert_extent_into_skyline (a1, b, X_AXIS, dir);
    }
}

vector<Skyline_entry>
empty_skyline (Direction d)
{
  vector<Skyline_entry> skyline;

  Interval i;
  i.set_empty ();
  i.swap ();
  Skyline_entry e;
  e.width_ = i;
  e.height_ = -d * infinity_f;
  skyline.push_back (e);
  return skyline;
}

vector<Skyline_entry>
extents_to_skyline (vector<Box> const &extents, Axis a, Direction d)
{

  vector<Skyline_entry> skyline = empty_skyline (d);

  /*
    This makes a cubic algorithm (array  insertion is O (n),
    searching the array dumbly is O (n), and for n items, we get O (n^3).)

    We could do a lot better (n log (n), using a balanced tree) but
    that seems overkill for now.
  */
  for (vsize j = extents.size (); j--;)
    insert_extent_into_skyline (&skyline, extents[j], a, d);

  return skyline;
}

/*
  minimum distance that can be achieved between baselines. "Clouds" is
  a skyline pointing down.

  This is an O (n) algorithm.
*/
Real
skyline_meshing_distance (vector<Skyline_entry> const &buildings,
			  vector<Skyline_entry> const &clouds)
{
  int i = buildings.size () -1;
  int j = clouds.size () -1;

  Real distance = -infinity_f;

  while (i > 0 || j > 0)
    {
      Interval w = buildings[i].width_;
      w.intersect (clouds[j].width_);

      if (!w.is_empty ())
	distance = max (distance, (buildings[i].height_ - clouds[j].height_));

      if (i > 0 && buildings[i].width_[LEFT] >= clouds[j].width_[LEFT])
	i--;
      else if (j > 0 && buildings[i].width_[LEFT] <= clouds[j].width_[LEFT])
	j--;
    }

  return distance;
}

Skyline_entry::Skyline_entry ()
{
  height_ = 0.0;
}

Skyline_entry::Skyline_entry (Interval i, Real r)
{
  width_ = i;
  height_ = r;
}

void
heighten_skyline (vector<Skyline_entry> *buildings, Real ground)
{
  for (vsize i = 0; i < buildings->size (); i++)
    buildings->at (i).height_ += ground;
}

Real
skyline_height (vector<Skyline_entry> const &buildings,
		Real airplane,
		Direction sky_dir)
{
  Real h = - sky_dir * infinity_f;

  /*
    Ugh! linear, should be O(log n).
   */
  for (vsize i = 0; i < buildings.size (); i++)
    if (buildings[i].width_.contains (airplane))
      h = sky_dir * max (sky_dir * h,
			 sky_dir * buildings[i].height_);
  
  return h;
}

