/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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

#include "skyline.hh"
#include "skyline-pair.hh"
#include "international.hh"

#include <deque>
#include <cstdio>

using std::deque;
using std::vector;

/* A skyline is a sequence of non-overlapping buildings: something like
   this:
                   _______
                  |       \                                 ________
                  |        \                       ________/        \
        /\        |          \                    /                  \
       /  --------             \                 /                    \
      /                          \              /                      \
     /                             ------------/                        ----
   --
   Each building has a starting position, and ending position, a starting
   height and an ending height.

   The following invariants are observed:
    - the start of the first building is at -infinity
    - the end of the last building is at infinity
    - if a building has infinite length (ie. the first and last buildings),
      then its starting height and ending height are equal
    - the end of one building is the same as the beginning of the next
      building

   We also allow skylines to point down (the structure is exactly the same,
   but we think of the part above the line as being filled with mass and the
   part below as being empty). ::distance finds the minimum distance between
   an UP skyline and a DOWN skyline.

   Note that we store DOWN skylines upside-down. That is, in order to compare
   a DOWN skyline with an UP skyline, we need to flip the DOWN skyline first.
   This means that the merging routine doesn't need to be aware of direction,
   but the distance routine does.

   From 2007 through 2012, buildings of width less than EPS were discarded,
   citing numerical accuracy concerns.  We remember that floating point
   comparisons of nearly-equal values can be affected by rounding error.
   Also, some target machines use the x87 floating point unit, which provides
   extended precision for intermediate results held in registers. On this type
   of hardware comparisons such as
     double c = 1.0/3.0; boolean compare = (c == 1.0/3.0)
   could go either way because the 1.0/3.0 is allowed to be kept
   higher precision than the variable 'c'.
   Alert to these considerations, we now accept buildings of zero-width.
*/

const char *const Skyline::type_p_name_ = "ly:skyline?";

static void
print_buildings (vector<Building> const &b)
{
  for (auto i : b)
    i.print ();
}

void
Skyline::print () const
{
  print_buildings (buildings_);
}

void
Skyline::print_points () const
{
  vector<Offset> ps (to_points (X_AXIS));

  int i = 0;
  for (Offset p : ps)
    printf ("(%f,%f)%s", p[X_AXIS], p[Y_AXIS], (i++ % 2) == 1 ? "\n" : " ");
}

Building::Building (Real start, Real start_height, Real end_height, Real end)
  : x_ (start, end)
{
  if (std::isinf (start) || std::isinf (end))
    assert (start_height == end_height);

  precompute (start_height, end_height);
}

Building::Building (Box const &b, Axis horizon_axis, Direction sky)
  : x_ (b[horizon_axis])
{
  Real height = sky * b[other_axis (horizon_axis)][sky];

  precompute (height, height);
}

void
Building::precompute (Real start_height, Real end_height)
{
  slope_
    = 0.0; /* if they were both infinite, we would get nan, not 0, from the prev line */
  if (start_height != end_height)
    slope_ = (end_height - start_height) / x_.length ();

  assert (std::isfinite (slope_));

  if (std::isinf (x_[LEFT]))
    {
      assert (start_height == end_height);
      y_intercept_ = start_height;
    }
  else if (fabs (slope_) > 1e6)
    // too steep to be stored in slope-intercept form, given round-off error
    {
      slope_ = 0.0;
      y_intercept_ = std::max (start_height, end_height);
    }
  else
    y_intercept_ = start_height - slope_ * x_[LEFT];
}

inline Real
Building::height (Real x) const
{
  return std::isinf (x) ? y_intercept_ : slope_ * x + y_intercept_;
}

void
Building::print () const
{
  printf ("%f x + %f from %f to %f\n", slope_, y_intercept_, x_[LEFT],
          x_[RIGHT]);
}

// Compute the abscissa at which these buildings intersect.  This is only
// meaningful if the two buildings have an intersection point (but we don't
// assert () it because numerical inaccuracies could make it false, although
// very "close" to true).
inline Real
Building::intersection_x (Building const &other) const
{
  Real slope_delta = other.slope_ - slope_;
  // If the slopes are really close (for example, if we happen to try merging
  // two identical buildings), avoid numerical inaccuracies related to dividing
  // by a small number.
  if (fabs (slope_delta) < 1e-4)
    return std::max (x_[LEFT], other.x_[LEFT]);
  return (y_intercept_ - other.y_intercept_) / slope_delta;
}

bool
Building::above (Building const &other, Real x) const
{
  return (std::isinf (y_intercept_) || std::isinf (other.y_intercept_)
          || std::isinf (x))
           ? y_intercept_ > other.y_intercept_
           : (slope_ - other.slope_) * x + y_intercept_ > other.y_intercept_;
}

void
Skyline::internal_merge_skyline (vector<Building> const *sbp,
                                 vector<Building> const *scp,
                                 vector<Building> *result) const
{
  if (sbp->empty () || scp->empty ())
    {
      programming_error ("tried to merge an empty skyline");
      return;
    }
  result->clear ();
  result->reserve (std::max (sbp->size (), scp->size ()));
  auto bit = sbp->begin ();
  auto cit = scp->begin ();

  Building b = *bit;
  for (; cit != scp->end (); ++cit)
    {
      /* Building b is continuing from the previous pass through the loop.
         Building c is newly-considered, and starts no earlier than b started.
         The comments draw b as if its roof had zero slope ----.
         with dashes where b lies above c.
         The roof of c could rise / or fall \ through the roof of b,
         or the vertical sides | of c could intersect the roof of b.  */
      Building c = *cit;
      if (b.x_[RIGHT] < c.x_[RIGHT]) /* finish with b */
        {
          if (b.x_[RIGHT] <= b.x_[LEFT]) /* we are already finished with b */
            ;
          else if (c.above (b, c.x_[LEFT])) /* -|   . | */
            {
              Building m (b);
              m.x_[RIGHT] = c.x_[LEFT];
              if (m.x_[RIGHT] > m.x_[LEFT])
                result->push_back (m);
              if (b.above (c, b.x_[RIGHT])) /* -|\--.   */
                {
                  Building n (c);
                  n.x_[RIGHT] = b.x_[LEFT] = b.intersection_x (c);
                  result->push_back (n);
                  result->push_back (b);
                  c.x_[LEFT] = b.x_[RIGHT];
                }
            }
          else
            {
              if (c.above (b, b.x_[RIGHT])) /* ---/ . | */
                c.x_[LEFT] = b.x_[RIGHT] = b.intersection_x (c);
              else /* -----.   */
                c.x_[LEFT] = b.x_[RIGHT];
              result->push_back (b);
            }
          /* 'c' continues further, so move it into 'b' for the next pass. */
          b = c;
          std::swap (bit, cit);
          std::swap (sbp, scp);
        }
      else /* b.x_[RIGHT] > c.x_[RIGHT] so finish with c */
        {
          if (c.above (b, c.x_[LEFT])) /* -| |---. */
            {
              Building m (b);
              m.x_[RIGHT] = c.x_[LEFT];
              if (m.x_[RIGHT] > m.x_[LEFT])
                result->push_back (m);
              if (b.above (c, c.x_[RIGHT])) /* -| \---. */
                c.x_[RIGHT] = b.intersection_x (c);
            }
          else if (c.above (b, c.x_[RIGHT])) /* ---/|--. */
            {
              Building m (b);
              c.x_[LEFT] = m.x_[RIGHT] = b.intersection_x (c);
              result->push_back (m);
            }
          else /* c is completely hidden by b */
            continue;
          result->push_back (c);
          b.x_[LEFT] = c.x_[RIGHT];
        }
    }
  if (b.x_[RIGHT] > b.x_[LEFT])
    result->push_back (b);
}

static void
empty_skyline (vector<Building> *const ret)
{
  assert (ret->empty ());
  ret->push_back (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f));
}

/*
  Given Building 'b', build a skyline containing only that building.
*/
static void
single_skyline (Building b, vector<Building> *const ret)
{
  assert (b.x_[RIGHT] >= b.x_[LEFT]);

  if (b.x_[LEFT] != -infinity_f)
    ret->push_back (
      Building (-infinity_f, -infinity_f, -infinity_f, b.x_[LEFT]));
  ret->push_back (b);
  if (b.x_[RIGHT] != infinity_f)
    ret->push_back (
      Building (b.x_[RIGHT], -infinity_f, -infinity_f, infinity_f));
}

/* Partition BUILDINGS into a non-overlapping set of boxes and the rest */
static void
non_overlapping_skyline (vector<Building> const &buildings,
                         vector<Building> *trimmed, vector<Building> *result)
{
  trimmed->reserve (buildings.size () / 2);
  result->reserve (buildings.size () / 2);
  Real last_end = -infinity_f;
  Building last_building (-infinity_f, -infinity_f, -infinity_f, infinity_f);
  for (auto const &b : buildings)
    {
      Real x1 = b.x_[LEFT];
      Real y1 = b.height (b.x_[LEFT]);
      Real x2 = b.x_[RIGHT];
      Real y2 = b.height (b.x_[RIGHT]);

      // Drop buildings that will obviously have no effect.
      if (last_building.height (x1) >= y1 && last_building.x_[RIGHT] >= x2
          && last_building.height (x2) >= y2)
        {
          continue;
        }

      if (x1 < last_end)
        {
          trimmed->push_back (b);
          continue;
        }

      // Insert empty Buildings into any gaps. (TODO: is this needed? -KOH)
      if (x1 > last_end)
        result->push_back (Building (last_end, -infinity_f, -infinity_f, x1));

      result->push_back (b);
      last_building = b;
      last_end = b.x_[RIGHT];
    }

  if (last_end < infinity_f)
    result->push_back (
      Building (last_end, -infinity_f, -infinity_f, infinity_f));
}

class LessThanBuilding
{
public:
  bool operator() (Building const &b1, Building const &b2)
  {
    return b1.x_[LEFT] < b2.x_[LEFT]
           || (b1.x_[LEFT] == b2.x_[LEFT]
               && b1.height (b1.x_[LEFT]) > b2.height (b1.x_[LEFT]));
  }
};

/**
   BUILDINGS is a list of buildings, but they could be overlapping
   and in any order.  The returned list of buildings is ordered and non-overlapping.
*/
vector<Building>
Skyline::internal_build_skyline (vector<Building> *buildings) const
{
  vsize size = buildings->size ();

  if (size == 0)
    {
      vector<Building> result;
      empty_skyline (&result);
      return result;
    }
  else if (size == 1)
    {
      vector<Building> result;
      single_skyline (buildings->front (), &result);
      return result;
    }

  deque<vector<Building>> partials;

  sort (buildings->begin (), buildings->end (), LessThanBuilding ());
  while (!buildings->empty ())
    {
      vector<Building> trimmed, partial;
      non_overlapping_skyline (*buildings, &trimmed, &partial);
      partials.push_back (partial);
      std::swap (*buildings, trimmed);
    }

  /* we'd like to say while (partials->size () > 1) but that's O (n).
     Instead, we exit in the middle of the loop */
  while (!partials.empty ())
    {
      vector<Building> one = partials.front ();
      partials.pop_front ();
      if (partials.empty ())
        return one;

      vector<Building> two = partials.front ();
      partials.pop_front ();

      vector<Building> merged;
      internal_merge_skyline (&one, &two, &merged);
      partials.push_back (merged);
    }
  assert (0);
  return vector<Building> ();
}

Skyline::Skyline ()
{
  sky_ = UP;
  empty_skyline (&buildings_);
}

Skyline::Skyline (Direction sky)
{
  sky_ = sky;
  empty_skyline (&buildings_);
}

/*
  Build skyline from a set of boxes.

  Boxes should be non-empty on both axes.  Otherwise, they will be ignored
 */
Skyline::Skyline (vector<Box> const &boxes, Axis horizon_axis, Direction sky)
{
  vector<Building> buildings;
  buildings.reserve (boxes.size ());
  sky_ = sky;

  for (vsize i = 0; i < boxes.size (); i++)
    if (!boxes[i].is_empty (X_AXIS) && !boxes[i].is_empty (Y_AXIS))
      buildings.push_back (Building (boxes[i], horizon_axis, sky));

  buildings_ = internal_build_skyline (&buildings);
}

/*
  build skyline from a set of line segments.

  Segments can be articulated from left to right or right to left.
  In the case of the latter, they will be stored internally as left to right.
 */
Skyline::Skyline (vector<Drul_array<Offset>> const &segments, Axis horizon_axis,
                  Direction sky)
{
  vector<Building> buildings;
  buildings.reserve (segments.size ());
  sky_ = sky;

  for (vsize i = 0; i < segments.size (); i++)
    {
      Drul_array<Offset> const &seg = segments[i];
      Offset left = seg[LEFT];
      Offset right = seg[RIGHT];
      if (left[horizon_axis] > right[horizon_axis])
        std::swap (left, right);

      Real x1 = left[horizon_axis];
      Real x2 = right[horizon_axis];
      Real y1 = left[other_axis (horizon_axis)] * sky;
      Real y2 = right[other_axis (horizon_axis)] * sky;

      if (x1 < x2)
        buildings.push_back (Building (x1, y1, y2, x2));
    }

  buildings_ = internal_build_skyline (&buildings);
}

Skyline::Skyline (vector<Skyline_pair> const &skypairs, Direction sky)
{
  sky_ = sky;

  deque<Skyline> partials;
  for (vsize i = 0; i < skypairs.size (); i++)
    partials.push_back (Skyline ((skypairs[i])[sky]));

  while (partials.size () > 1)
    {
      Skyline one = partials.front ();
      partials.pop_front ();
      Skyline two = partials.front ();
      partials.pop_front ();

      one.merge (two);
      partials.push_back (one);
    }

  if (partials.size ())
    buildings_.swap (partials.front ().buildings_);
  else
    buildings_.clear ();
}

Skyline::Skyline (Box const &b, Axis horizon_axis, Direction sky)
{
  sky_ = sky;
  if (!b.is_empty (X_AXIS) && !b.is_empty (Y_AXIS))
    {
      Building front (b, horizon_axis, sky);
      single_skyline (front, &buildings_);
    }
}

void
Skyline::merge (Skyline const &other)
{
  assert (sky_ == other.sky_);

  if (other.is_empty ())
    return;

  if (is_empty ())
    {
      buildings_ = other.buildings_;
      return;
    }

  vector<Building> other_bld (other.buildings_);
  vector<Building> dest;
  internal_merge_skyline (&other_bld, &buildings_, &dest);
  dest.swap (buildings_);
}

void
Skyline::raise (Real r)
{
  for (auto i = buildings_.begin (); i != buildings_.end (); i++)
    i->y_intercept_ += sky_ * r;
}

void
Skyline::shift (Real s)
{
  for (auto i = buildings_.begin (); i != buildings_.end (); i++)
    {
      i->x_[LEFT] += s;
      i->x_[RIGHT] += s;
      i->y_intercept_ -= s * i->slope_;
    }
}

Real
Skyline::distance (Skyline const &other, Real horizon_padding) const
{
  Real dummy;
  return internal_distance (other, horizon_padding, &dummy);
}

Real
Skyline::touching_point (Skyline const &other, Real horizon_padding) const
{
  Real touch;
  internal_distance (other, horizon_padding, &touch);
  return touch;
}

Real
Skyline::internal_distance (Skyline const &other, Real horizon_padding,
                            Real *touch_point) const
{
  if (horizon_padding == 0.0)
    return internal_distance (other, touch_point);

  // Note that it is not necessary to build a padded version of other,
  // because the same effect can be achieved just by doubling horizon_padding.
  Skyline padded_this = padded (horizon_padding);
  return padded_this.internal_distance (other, touch_point);
}

Skyline
Skyline::padded (Real horizon_padding) const
{
  if (horizon_padding < 0.0)
    warning (_ ("Cannot have negative horizon padding.  Junking."));

  if (horizon_padding <= 0.0)
    return *this;

  vector<Building> pad_buildings;
  pad_buildings.reserve (4 * buildings_.size ());
  for (auto const &b : buildings_)
    {
      if (b.x_[LEFT] > -infinity_f)
        {
          Real height = b.height (b.x_[LEFT]);
          if (height > -infinity_f)
            {
              // Add the sloped building that pads the left side of the current building.
              Real start = b.x_[LEFT] - 2 * horizon_padding;
              Real end = b.x_[LEFT] - horizon_padding;
              pad_buildings.push_back (
                Building (start, height - horizon_padding, height, end));

              // Add the flat building that pads the left side of the current building.
              start = b.x_[LEFT] - horizon_padding;
              end = b.x_[LEFT];
              pad_buildings.push_back (Building (start, height, height, end));
            }
        }

      if (b.x_[RIGHT] < infinity_f)
        {
          Real height = b.height (b.x_[RIGHT]);
          if (height > -infinity_f)
            {
              // Add the flat building that pads the right side of the current building.
              Real start = b.x_[RIGHT];
              Real end = start + horizon_padding;
              pad_buildings.push_back (Building (start, height, height, end));

              // Add the sloped building that pads the right side of the current building.
              start = end;
              end += horizon_padding;
              pad_buildings.push_back (
                Building (start, height, height - horizon_padding, end));
            }
        }
    }

  // The buildings may be overlapping, so resolve that.
  vector<Building> pad_skyline = internal_build_skyline (&pad_buildings);

  // Merge the padding with the original, to make a new skyline.
  Skyline padded (sky_);
  internal_merge_skyline (&pad_skyline, &buildings_, &padded.buildings_);

  return padded;
}

Real
Skyline::internal_distance (Skyline const &other, Real *touch_point) const
{
  assert (sky_ == -other.sky_);

  auto i = buildings_.begin ();
  auto j = other.buildings_.begin ();

  Real dist = -infinity_f;
  Real start = -infinity_f;
  Real touch = -infinity_f;
  while (i != buildings_.end () && j != other.buildings_.end ())
    {
      Real end = std::min (i->x_[RIGHT], j->x_[RIGHT]);
      Real start_dist = i->height (start) + j->height (start);
      Real end_dist = i->height (end) + j->height (end);
      dist = std::max (dist, std::max (start_dist, end_dist));

      if (end_dist == dist)
        touch = end;
      else if (start_dist == dist)
        touch = start;

      if (i->x_[RIGHT] <= j->x_[RIGHT])
        i++;
      else
        j++;
      start = end;
    }

  *touch_point = touch;
  return dist;
}

static bool
building_on_left_of (Building const &b, Real limit)
{
  return b.x_[RIGHT] < limit;
}

Real
Skyline::height (Real x) const
{
  assert (!std::isinf (x));
  auto relevant_building = std::lower_bound (
    buildings_.begin (), buildings_.end (), x, building_on_left_of);
  assert (relevant_building != buildings_.end ());
  return sky_ * relevant_building->height (x);
}

Real
Skyline::max_height () const
{
  Real ret = -infinity_f;

  for (auto const &b : buildings_)
    {
      // TODO: unnecessary calculations.
      ret = std::max (ret, b.height (b.x_[LEFT]));
      ret = std::max (ret, b.height (b.x_[RIGHT]));
    }

  return sky_ * ret;
}

Direction
Skyline::direction () const
{
  return sky_;
}

// X of first building in skyline
Real
Skyline::left () const
{
  for (auto const &b : buildings_)
    if (b.y_intercept_ > -infinity_f)
      return b.x_[LEFT];

  return infinity_f;
}

// X of end of last building in skyline
Real
Skyline::right () const
{
  for (auto i = buildings_.rbegin (); i != buildings_.rend (); ++i)
    if (i->y_intercept_ > -infinity_f)
      return i->x_[RIGHT];

  return -infinity_f;
}

Real
Skyline::max_height_position () const
{
  Skyline s (-sky_);
  // TODO: should be able to calc without doing a merge?
  s.set_minimum_height (0);
  return touching_point (s);
}

void
Skyline::set_minimum_height (Real h)
{
  Skyline s (sky_);
  s.buildings_.front ().y_intercept_ = h * sky_;
  merge (s);
}

vector<Offset>
Skyline::to_points (Axis horizon_axis) const
{
  vector<Offset> out;
  out.reserve (2 * buildings_.size ());

  for (auto const &b : buildings_)
    {
      out.push_back (Offset (b.x_[LEFT], sky_ * b.height (b.x_[LEFT])));
      out.push_back (Offset (b.x_[RIGHT], sky_ * b.height (b.x_[RIGHT])));
    }

  if (horizon_axis == Y_AXIS)
    for (vsize i = 0; i < out.size (); i++)
      out[i] = out[i].swapped ();

  return out;
}

bool
Skyline::is_empty () const
{
  if (!buildings_.size ())
    return true;
  Building b = buildings_.front ();
  return b.x_[RIGHT] == infinity_f && b.y_intercept_ == -infinity_f;
}

void
Skyline::clear ()
{
  buildings_.clear ();
  empty_skyline (&buildings_);
}
