/*   
  skyline.hh -- declare Skyline_entry and funcbs.

  source file of the GNU LilyPond music typesetter

  (c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef SKYLINE_HH
#define SKYLINE_HH

#include "array.hh"
#include "box.hh"

struct Skyline_entry
{
  Interval width_;
  Real height_;
  Skyline_entry();
  Skyline_entry (Interval, Real);
};

void
insert_extent_into_skyline (Array<Skyline_entry> *line, Box b, Axis line_axis,
			    Direction d);
Array<Skyline_entry>
extents_to_skyline (Array<Box> extents, Axis a, Direction d);
Real
skyline_meshing_distance (Array<Skyline_entry> buildings,
			  Array<Skyline_entry> clouds);


#endif /* SKYLINE_HH */

