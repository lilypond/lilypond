/*
  skyline.hh -- declare Skyline_entry and funcbs.

  source file of the GNU LilyPond music typesetter

  (c) 2002--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
merge_skyline (std::vector<Skyline_entry> *a1, std::vector<Skyline_entry> const &a2,
	       Direction);
void insert_extent_into_skyline (std::vector<Skyline_entry> *line, Box b, Axis line_axis,
				 Direction d);
std::vector<Skyline_entry>
extents_to_skyline (std::vector<Box> const &extents, Axis a, Direction d);
std::vector<Skyline_entry> empty_skyline (Direction d);
void heighten_skyline (std::vector<Skyline_entry> *buildings, Real ground);
Real
skyline_meshing_distance (std::vector<Skyline_entry> const &buildings,
			  std::vector<Skyline_entry> const &clouds);

Real
skyline_height (std::vector<Skyline_entry> const &buildings,
		Real airplane, Direction sky_dir);

#endif /* SKYLINE_HH */

