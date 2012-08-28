/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2012 Joe Neeman <joeneeman@gmail.com>

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
#include <deque>
#include <cstdio>

#include "ly-smobs.icc"

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
*/

/* If we start including very thin buildings, numerical accuracy errors can
   arise. Therefore, we ignore all buildings that are less than epsilon wide. */
#define EPS 1e-5

static void
print_buildings (list<Building> const &b)
{
  for (list<Building>::const_iterator i = b.begin (); i != b.end (); i++)
    i->print ();
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

  for (vsize i = 0; i < ps.size (); i++)
    printf ("(%f,%f)%s", ps[i][X_AXIS], ps[i][Y_AXIS],
            (i % 2) == 1 ? "\n" : " ");
}

Building::Building (Real start, Real start_height, Real end_height, Real end)
{
  if (isinf (start) || isinf (end))
    assert (start_height == end_height);

  start_ = start;
  end_ = end;
  precompute (start, start_height, end_height, end);
}

Building::Building (Box const &b, Axis horizon_axis, Direction sky)
{
  Real start = b[horizon_axis][LEFT];
  Real end = b[horizon_axis][RIGHT];
  Real height = sky * b[other_axis (horizon_axis)][sky];

  start_ = start;
  end_ = end;
  precompute (start, height, height, end);
}

void
Building::precompute (Real start, Real start_height, Real end_height, Real end)
{
  slope_ = 0.0; /* if they were both infinite, we would get nan, not 0, from the prev line */
  if (start_height != end_height)
    slope_ = (end_height - start_height) / (end - start);

  assert (!isinf (slope_) && !isnan (slope_));

  if (isinf (start))
    {
      assert (start_height == end_height);
      y_intercept_ = start_height;
    }
  else
    y_intercept_ = start_height - slope_ * start;
}

inline Real
Building::height (Real x) const
{
  return isinf (x) ? y_intercept_ : slope_ * x + y_intercept_;
}

void
Building::print () const
{
  printf ("%f x + %f from %f to %f\n", slope_, y_intercept_, start_, end_);
}

inline Real
Building::intersection_x (Building const &other) const
{
  Real ret = (y_intercept_ - other.y_intercept_) / (other.slope_ - slope_);
  return isnan (ret) ? -infinity_f : ret;
}

void
Building::leading_part (Real chop)
{
  assert (chop <= end_);
  end_ = chop;
}

// Returns a shift s such that (x + s, y) intersects the roof of
// this building.  If no such shift exists, returns infinity_f.
Real
Building::shift_to_intersect (Real x, Real y) const
{
  // Solve for s: y = (x + s)*m + b
  Real ret = (y - y_intercept_ - slope_ * x) / slope_;

  if (ret >= start_ && ret <= end_ && !isinf (ret))
    return ret;
  return infinity_f;
}

// Returns the interval of horizontal shifts for which this
// building (pointing up) overlaps the other building (pointing down).
Interval
Building::overlapping_shift_interval (Building const &other) const
{
  Interval iv;

  // If one building is empty, there will never be an overlap.
  if (y_intercept_ == -infinity_f || other.y_intercept_ == -infinity_f)
    return iv;

  // There are two kinds of interesting positions:
  // - when the horizontal extents of the buildings just touch
  // - when an endpoint of one building intersects the roof of the other.
  // The interval we are looking for is the smallest one that
  // contains all of the interesting points.


  Real my_y1 = height (start_);
  Real my_y2 = height (end_);
  Real his_y1 = -other.height (other.start_); // "-" because OTHER points down
  Real his_y2 = -other.height (other.end_);

  // If both buildings are infinite in the same direction,
  // the return value is either empty or full.
  if ((isinf (start_) && isinf (other.start_))
      || (isinf (end_) && isinf (other.end_)))
    return (y_intercept_ > other.y_intercept_)
           ? Interval (-infinity_f, infinity_f) : Interval ();

  // ...when the horizontal extents of the buildings just touch...
  if (my_y1 >= his_y2)
    iv.add_point (other.end_ - start_);
  if (my_y2 >= his_y1)
    iv.add_point (other.start_ - end_);

  // ...when an endpoint of one building intersects the roof of the other.
  Real p1 = shift_to_intersect (other.start_, his_y1);
  Real p2 = shift_to_intersect (other.end_, his_y2);
  // "-my_y1" because OTHER points down:
  Real p3 = other.shift_to_intersect (start_, -my_y1);
  Real p4 = other.shift_to_intersect (end_, -my_y2);
  if (!isinf (p1))
    iv.add_point (p1);
  if (!isinf (p2))
    iv.add_point (p2);
  if (!isinf (p3))
    iv.add_point (p3);
  if (!isinf (p4))
    iv.add_point (p4);

  return iv;
}

static Real
first_intersection (Building const &b, list<Building> *const s, Real start_x)
{
  while (!s->empty () && start_x < b.end_)
    {
      Building c = s->front ();

      // conceals and intersection_x involve multiplication and
      // division. Avoid that, if we can.
      if (c.y_intercept_ == -infinity_f)
        {
          if (c.end_ > b.end_)
            return b.end_;
          start_x = c.end_;
          s->pop_front ();
          continue;
        }

      if (c.conceals (b, start_x))
        return start_x;

      Real i = b.intersection_x (c);
      if (i > start_x && i <= b.end_ && i <= c.end_)
        return i;

      start_x = c.end_;
      if (b.end_ > c.end_)
        s->pop_front ();
    }
  return b.end_;
}

bool
Building::conceals (Building const &other, Real x) const
{
  if (slope_ == other.slope_)
    return y_intercept_ > other.y_intercept_;

  /* their slopes were not equal, so there is an intersection point */
  Real i = intersection_x (other);
  return (i <= x && slope_ > other.slope_)
         || (i > x && slope_ < other.slope_);
}

// Remove redundant empty buildings from the skyline.
// If there are two adjacent empty buildings, they can be
// turned into one.
void
Skyline::normalize ()
{
  bool last_empty = false;
  list<Building>::iterator i;
  for (i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (last_empty && i->y_intercept_ == -infinity_f)
        {
          list<Building>::iterator last = i;
          last--;
          last->end_ = i->end_;
          buildings_.erase (i);
          i = last;
        }
      last_empty = (i->y_intercept_ == -infinity_f);
    }

  assert (buildings_.front ().start_ == -infinity_f);
  assert (buildings_.back ().end_ == infinity_f);
}

void
Skyline::deholify ()
{
  // Since a skyline should always be normalized, we can
  // assume that there are never two adjacent empty buildings.
  // That is, if center is empty then left and right are not.
  list<Building>::iterator left = buildings_.begin ();
  list<Building>::iterator center = buildings_.begin ();
  list<Building>::iterator right;

  for (right = buildings_.begin (); right != buildings_.end (); right++)
    {
      if (center != buildings_.begin () && center->y_intercept_ == -infinity_f)
        {
          Real p1 = left->height (left->end_);
          Real p2 = right->height (right->start_);
          *center = Building (center->start_, p1, p2, center->end_);

          left = center;
          center = right;
        }
    }
}

void
Skyline::internal_merge_skyline (list<Building> *s1, list<Building> *s2,
                                 list<Building> *const result) const
{
  if (s1->empty () || s2->empty ())
    {
      programming_error ("tried to merge an empty skyline");
      return;
    }

  Real x = -infinity_f;
  Real last_end = -infinity_f;
  while (!s1->empty ())
    {
      if (s2->front ().conceals (s1->front (), x))
        swap (s1, s2);

      Building b = s1->front ();
      Building c = s2->front ();

      // Optimization: if the other skyline is empty at this point,
      // we can avoid testing some intersections. Just grab as many
      // buildings from s1 as we can, and shove them onto the output.
      if (c.y_intercept_ == -infinity_f
          && c.end_ >= b.end_)
        {
          list<Building>::iterator i = s1->begin ();
          i++;
          while (i != s1->end () && i->end_ <= c.end_)
            i++;

          s1->front ().start_ = x;
          result->splice (result->end (), *s1, s1->begin (), i);
          x = result->back ().end_;
          last_end = x;
          continue;
        }

      Real end = first_intersection (b, s2, x);
      if (s2->empty ())
        {
          b.start_ = last_end;
          result->push_back (b);
          break;
        }

      /* only include buildings wider than epsilon */
      if (end > x + EPS)
        {
          b.leading_part (end);
          b.start_ = last_end;
          last_end = b.end_;
          result->push_back (b);
        }

      if (end >= s1->front ().end_)
        s1->pop_front ();

      x = end;
    }
}

static void
empty_skyline (list<Building> *const ret)
{
  ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f));
}

/*
  Given Building 'b', build a skyline containing only that building.
*/
static void
single_skyline (Building b, list<Building> *const ret)
{
  if (b.end_ > b.start_ + EPS)
    {
      ret->push_back (Building (-infinity_f, -infinity_f,
                                -infinity_f, b.start_));
      ret->push_back (b);
      ret->push_back (Building (b.end_, -infinity_f,
                                -infinity_f, infinity_f));
    }
  else
    {
      empty_skyline (ret);
    }
}

/* remove a non-overlapping set of boxes from BOXES and build a skyline
   out of them */
static list<Building>
non_overlapping_skyline (list<Building> *const buildings)
{
  list<Building> result;
  Real last_end = -infinity_f;
  Building last_building (-infinity_f, -infinity_f, -infinity_f, infinity_f);
  list<Building>::iterator i = buildings->begin ();
  while (i != buildings->end ())
    {
      Real x1 = i->start_;
      Real y1 = i->height (i->start_);
      Real x2 = i->end_;
      Real y2 = i->height (i->end_);

      // Drop buildings that will obviously have no effect.
      if (last_building.height (x1) >= y1
          && last_building.end_ >= x2
          && last_building.height (x2) >= y2)
        {
          list<Building>::iterator j = i++;
          buildings->erase (j);
          continue;
        }

      if (x1 < last_end)
        {
          i++;
          continue;
        }

      if (x1 > last_end + EPS)
        result.push_back (Building (last_end, -infinity_f, -infinity_f, x1));

      result.push_back (*i);
      last_building = *i;
      last_end = i->end_;

      list<Building>::iterator j = i++;
      buildings->erase (j);
    }

  if (last_end < infinity_f)
    result.push_back (Building (last_end, -infinity_f, -infinity_f, infinity_f));
  return result;
}

class LessThanBuilding
{
public:
  bool operator () (Building const &b1, Building const &b2)
  {
    return b1.start_ < b2.start_
           || (b1.start_ == b2.start_ && b1.height (b1.start_) > b2.height (b1.start_));
  }
};

/**
   BUILDINGS is a list of buildings, but they could be overlapping
   and in any order.  The returned list of buildings is ordered and non-overlapping.
*/
list<Building>
Skyline::internal_build_skyline (list<Building> *buildings) const
{
  vsize size = buildings->size ();

  if (size == 0)
    {
      list<Building> result;
      empty_skyline (&result);
      return result;
    }
  else if (size == 1)
    {
      list<Building> result;
      single_skyline (buildings->front (), &result);
      return result;
    }

  deque<list<Building> > partials;
  buildings->sort (LessThanBuilding ());
  while (!buildings->empty ())
    partials.push_back (non_overlapping_skyline (buildings));

  /* we'd like to say while (partials->size () > 1) but that's O (n).
     Instead, we exit in the middle of the loop */
  while (!partials.empty ())
    {
      list<Building> merged;
      list<Building> one = partials.front ();
      partials.pop_front ();
      if (partials.empty ())
        return one;

      list<Building> two = partials.front ();
      partials.pop_front ();
      internal_merge_skyline (&one, &two, &merged);
      partials.push_back (merged);
    }
  assert (0);
  return list<Building> ();
}

Skyline::Skyline ()
{
  sky_ = UP;
  empty_skyline (&buildings_);
}

Skyline::Skyline (Skyline const &src)
{
  sky_ = src.sky_;

  /* doesn't a list's copy constructor do this? -- jneem */
  for (list<Building>::const_iterator i = src.buildings_.begin ();
       i != src.buildings_.end (); i++)
    {
      buildings_.push_back (Building ((*i)));
    }
}

Skyline::Skyline (Direction sky)
{
  sky_ = sky;
  empty_skyline (&buildings_);
}

/*
  Build skyline from a set of boxes.

  Boxes should have fatness in the horizon_axis, otherwise they are ignored.
 */
Skyline::Skyline (vector<Box> const &boxes, Axis horizon_axis, Direction sky)
{
  list<Building> buildings;
  sky_ = sky;

  Axis vert_axis = other_axis (horizon_axis);
  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][horizon_axis];
      if (iv.length () > EPS && !boxes[i][vert_axis].is_empty ())
        buildings.push_front (Building (boxes[i], horizon_axis, sky));
    }

  buildings_ = internal_build_skyline (&buildings);
  normalize ();
}

/*
  build skyline from a set of line segments.

  Buildings should have fatness in the horizon_axis, otherwise they are ignored.
 */
Skyline::Skyline (vector<Drul_array<Offset> > const &segments, Axis horizon_axis, Direction sky)
{
  list<Building> buildings;
  sky_ = sky;

  for (vsize i = 0; i < segments.size (); i++)
    {
      Drul_array<Offset> const &seg = segments[i];
      Offset left = seg[LEFT];
      Offset right = seg[RIGHT];
      if (left[horizon_axis] > right[horizon_axis])
        swap (left, right);

      Real x1 = left[horizon_axis];
      Real x2 = right[horizon_axis];
      Real y1 = left[other_axis (horizon_axis)] * sky;
      Real y2 = right[other_axis (horizon_axis)] * sky;

      if (x1 + EPS < x2)
        buildings.push_back (Building (x1, y1, y2, x2));
    }

  buildings_ = internal_build_skyline (&buildings);
  normalize ();
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
  Building front (b, horizon_axis, sky);
  single_skyline (front, &buildings_);
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

  list<Building> other_bld (other.buildings_);
  list<Building> my_bld;
  my_bld.splice (my_bld.begin (), buildings_);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
  normalize ();
}

void
Skyline::insert (Box const &b, Axis a)
{
  list<Building> other_bld;
  list<Building> my_bld;

  if (isnan (b[other_axis (a)][LEFT])
      || isnan (b[other_axis (a)][RIGHT]))
    {
      programming_error ("insane box for skyline");
      return;
    }

  /* do the same filtering as in Skyline (vector<Box> const&, etc.) */
  Interval iv = b[a];
  if (iv.length () <= EPS || b[other_axis (a)].is_empty ())
    return;

  my_bld.splice (my_bld.begin (), buildings_);
  single_skyline (Building (b, a, sky_), &other_bld);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
  normalize ();
}

void
Skyline::raise (Real r)
{
  list<Building>::iterator end = buildings_.end ();
  for (list<Building>::iterator i = buildings_.begin (); i != end; i++)
    i->y_intercept_ += sky_ * r;
}

void
Skyline::shift (Real s)
{
  list<Building>::iterator end = buildings_.end ();
  for (list<Building>::iterator i = buildings_.begin (); i != end; i++)
    {
      i->start_ += s;
      i->end_ += s;
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
Skyline::internal_distance (Skyline const &other, Real horizon_padding, Real *touch_point) const
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
  list<Building> pad_buildings;
  for (list<Building>::const_iterator i = buildings_.begin (); i != buildings_.end (); ++i)
    {
      if (i->start_ > -infinity_f)
        {
          Real height = i->height (i->start_);
          if (height > -infinity_f)
            {
              // Add the sloped building that pads the left side of the current building.
              Real start = i->start_ - 2 * horizon_padding;
              Real end = i->start_ - horizon_padding;
              pad_buildings.push_back (Building (start, height - horizon_padding, height, end));

              // Add the flat building that pads the left side of the current building.
              start = i->start_ - horizon_padding;
              end = i->start_;
              pad_buildings.push_back (Building (start, height, height, end));
            }
        }

      if (i->end_ < infinity_f)
        {
          Real height = i->height (i->end_);
          if (height > -infinity_f)
            {
              // Add the flat building that pads the right side of the current building.
              Real start = i->end_;
              Real end = start + horizon_padding;
              pad_buildings.push_back (Building (start, height, height, end));

              // Add the sloped building that pads the right side of the current building.
              start = end;
              end += horizon_padding;
              pad_buildings.push_back (Building (start, height, height - horizon_padding, end));
            }
        }
    }

  // The buildings may be overlapping, so resolve that.
  list<Building> pad_skyline = internal_build_skyline (&pad_buildings);

  // Merge the padding with the original, to make a new skyline.
  Skyline padded (sky_);
  list<Building> my_buildings = buildings_;
  padded.buildings_.clear ();
  internal_merge_skyline (&pad_skyline, &my_buildings, &padded.buildings_);
  padded.normalize ();

  return padded;
}

Real
Skyline::internal_distance (Skyline const &other, Real *touch_point) const
{
  assert (sky_ == -other.sky_);

  list<Building>::const_iterator i = buildings_.begin ();
  list<Building>::const_iterator j = other.buildings_.begin ();

  Real dist = -infinity_f;
  Real start = -infinity_f;
  Real touch = -infinity_f;
  while (i != buildings_.end () && j != other.buildings_.end ())
    {
      Real end = min (i->end_, j->end_);
      Real start_dist = i->height (start) + j->height (start);
      Real end_dist = i->height (end) + j->height (end);
      dist = max (dist, max (start_dist, end_dist));

      if (end_dist == dist)
        touch = end;
      else if (start_dist == dist)
        touch = start;

      if (i->end_ <= j->end_)
        i++;
      else
        j++;
      start = end;
    }

  *touch_point = touch;
  return dist;
}

Real
Skyline::height (Real airplane) const
{
  assert (!isinf (airplane));

  list<Building>::const_iterator i;
  for (i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (i->end_ >= airplane)
        return sky_ * i->height (airplane);
    }

  assert (0);
  return 0;
}

Real
Skyline::max_height () const
{
  Real ret = -infinity_f;

  list<Building>::const_iterator i;
  for (i = buildings_.begin (); i != buildings_.end (); ++i)
    {
      ret = max (ret, i->height (i->start_));
      ret = max (ret, i->height (i->end_));
    }

  return sky_ * ret;
}

Real
Skyline::max_height_position () const
{
  Skyline s (-sky_);
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

  Real start = -infinity_f;
  for (list<Building>::const_iterator i (buildings_.begin ());
       i != buildings_.end (); i++)
    {
      out.push_back (Offset (start, sky_ * i->height (start)));
      out.push_back (Offset (i->end_, sky_ * i->height (i->end_)));
      start = i->end_;
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
  return b.end_ == infinity_f && b.y_intercept_ == -infinity_f;
}

void
Skyline::clear ()
{
  buildings_.clear ();
  empty_skyline (&buildings_);
}

/****************************************************************/

IMPLEMENT_SIMPLE_SMOBS (Skyline);
IMPLEMENT_TYPE_P (Skyline, "ly:skyline?");
IMPLEMENT_DEFAULT_EQUAL_P (Skyline);

SCM
Skyline::mark_smob (SCM s)
{
  ASSERT_LIVE_IS_ALLOWED (s);
  return SCM_EOL;
}

int
Skyline::print_smob (SCM s, SCM port, scm_print_state *)
{
  Skyline *r = (Skyline *) SCM_CELL_WORD_1 (s);
  (void) r;

  scm_puts ("#<Skyline>", port);

  return 1;
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Skyline, get_touching_point, 3, 1, "")
SCM
Skyline::get_touching_point (SCM skyline_scm, SCM other_skyline_scm, SCM horizon_padding_scm)
{
  LY_ASSERT_SMOB (Skyline, other_skyline_scm, 1);

  Real horizon_padding = 0;
  if (horizon_padding_scm != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_number, horizon_padding_scm, 3);
      horizon_padding = scm_to_double (horizon_padding_scm);
    }

  Skyline *skyline = Skyline::unsmob (skyline_scm);
  Skyline *other_skyline = Skyline::unsmob (other_skyline_scm);
  return scm_from_double (skyline->touching_point (*other_skyline, horizon_padding));
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Skyline, get_distance, 3, 1, "")
SCM
Skyline::get_distance (SCM skyline_scm, SCM other_skyline_scm, SCM horizon_padding_scm)
{
  LY_ASSERT_SMOB (Skyline, other_skyline_scm, 1);

  Real horizon_padding = 0;
  if (horizon_padding_scm != SCM_UNDEFINED)
    {
      LY_ASSERT_TYPE (scm_is_number, horizon_padding_scm, 3);
      horizon_padding = scm_to_double (horizon_padding_scm);
    }

  Skyline *skyline = Skyline::unsmob (skyline_scm);
  Skyline *other_skyline = Skyline::unsmob (other_skyline_scm);
  return scm_from_double (skyline->distance (*other_skyline, horizon_padding));
}

MAKE_SCHEME_CALLBACK (Skyline, get_max_height, 1)
SCM
Skyline::get_max_height (SCM skyline_scm)
{
  return scm_from_double (Skyline::unsmob (skyline_scm)->max_height ());
}

MAKE_SCHEME_CALLBACK (Skyline, get_max_height_position, 1)
SCM
Skyline::get_max_height_position (SCM skyline_scm)
{
  return scm_from_double (Skyline::unsmob (skyline_scm)->max_height_position ());
}

MAKE_SCHEME_CALLBACK (Skyline, get_height, 2)
SCM
Skyline::get_height (SCM skyline_scm, SCM x_scm)
{
  Real x = robust_scm2double (x_scm, 0.0);
  return scm_from_double (Skyline::unsmob (skyline_scm)->height (x));
}
