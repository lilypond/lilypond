/*
  skyline.hh -- declare Skyline_entry and funcbs.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SKYLINE_HH
#define SKYLINE_HH

#include "std-vector.hh"
#include "box.hh"

struct Skyline_entry
{
  Interval width_;
  Real height_;
  Skyline_entry ();
  Skyline_entry (Interval, Real);
};

void
merge_skyline (vector<Skyline_entry> *a1, vector<Skyline_entry> const &a2,
	       Direction);
void insert_extent_into_skyline (vector<Skyline_entry> *line, Box b, Axis line_axis,
				 Direction d);
vector<Skyline_entry>
extents_to_skyline (vector<Box> const &extents, Axis a, Direction d);
vector<Skyline_entry> empty_skyline (Direction d);
void heighten_skyline (vector<Skyline_entry> *buildings, Real ground);
Real
skyline_meshing_distance (vector<Skyline_entry> const &buildings,
			  vector<Skyline_entry> const &clouds);

Real
skyline_height (vector<Skyline_entry> const &buildings,
		Real airplane, Direction sky_dir);

#endif /* SKYLINE_HH */

