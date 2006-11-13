/*
  skyline.hh -- declare Skyline class.

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef SKYLINE_HH
#define SKYLINE_HH

#include <list>
#include "axis.hh"
#include "box.hh"
#include "interval.hh"
#include "direction.hh"
#include "std-vector.hh"

struct Building
{
  Interval iv_;
  Real start_height_;
  Real end_height_;
  Real m_;
  Real b_;

  Building (Real start, Real start_height, Real end_height, Real end);

  Real height (Real x) const;
  Real intersection (Building const &other) const;
  void leading_part (Real chop);
  bool obstructs (Building const &other) const;
};

class Skyline
{
public:
private:
  list<Building> buildings_;
  Direction sky_;
  Real last_visible_point (Building const &b, list<Building> *const sky);
  void internal_merge_skyline (list<Building>*, list<Building>*,
			       list<Building> *const result);
  void internal_build_skyline (list<Building>*,
			       list<Building> *const result);
  bool is_legal_skyline () const;

public:
  Skyline ();
  Skyline (Direction sky);
  Skyline (vector<Box> const &bldgs, Axis a, Direction sky);

  void merge (Skyline const &);
  void insert (Box const &, Axis);
  void raise (Real);
  Real distance (Skyline const &) const;
  Real height (Real airplane) const;
  Real max_height () const;
  void set_minimum_height (Real height);
};

#endif /* SKYLINE_HH */

