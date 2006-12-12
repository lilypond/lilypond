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
#include "smobs.hh"

struct Building
{
  Interval iv_;
  Drul_array<Real> height_;

  Real y_intercept_;
  Real slope_;

  void precompute ();
  Building (Real start, Real start_height, Real end_height, Real end);
  void print () const;

  Real height (Real x) const;
  Real intersection (Building const &other) const;
  void leading_part (Real chop);
  bool conceals_beginning (Building const &other) const;
  bool conceals (Building const &other) const;
};

class Skyline
{
private:
  list<Building> buildings_;
  Direction sky_;
  
  void internal_merge_skyline (list<Building>*, list<Building>*,
			       list<Building> *const result);
  void internal_build_skyline (list<Building>*, list<Building> *const result);
  bool is_legal_skyline () const;

  DECLARE_SIMPLE_SMOBS(Skyline);
public:
  Skyline ();
  Skyline (Skyline const &src);
  Skyline (Direction sky);
  Skyline (vector<Box> const &bldgs, Axis a, Direction sky);
  Skyline (vector<Offset> const &points, Direction sky);
  vector<Offset> to_points () const;
  void merge (Skyline const &);
  void insert (Box const &, Axis);
  void print () const;
  void raise (Real);
  Real distance (Skyline const &) const;
  Real height (Real airplane) const;
  Real max_height () const;
  void set_minimum_height (Real height);
};

#endif /* SKYLINE_HH */

