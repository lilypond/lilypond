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

  end_ = end;
  precompute (start, start_height, end_height, end);
}

Building::Building (Box const &b, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  Real start = b[horizon_axis][LEFT] - horizon_padding;
  Real end = b[horizon_axis][RIGHT] + horizon_padding;
  Real height = sky * b[other_axis (horizon_axis)][sky];

  end_ = end;
  precompute (start, height, height, end);
}

void
Building::precompute (Real start, Real start_height, Real end_height, Real end)
{
  slope_ = (end_height - start_height) / (end - start);
  if (start_height == end_height) /* if they were both infinite, we would get nan, not 0, from the prev line */
    slope_ = 0;

  assert (!isinf (slope_) && !isnan (slope_));

  if (isinf (start))
    {
      assert (start_height == end_height);
      y_intercept_ = start_height;
    }
  else
    y_intercept_ = start_height - slope_ * start;
}

Real
Building::height (Real x) const
{
  return isinf (x) ? y_intercept_ : slope_ * x + y_intercept_;
}

void
Building::print () const
{
  printf ("%f x + %f ends at %f\n", slope_, y_intercept_, end_);
}

Real
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

Building
Building::sloped_neighbour (Real start, Real horizon_padding, Direction d) const
{
  Real x = (d == LEFT) ? start : end_;
  Real left = x;
  Real right = x + d * horizon_padding;
  Real left_height = height (x);
  Real right_height = left_height - horizon_padding;
  if (d == LEFT)
    {
      swap (left, right);
      swap (left_height, right_height);
    }
  return Building (left, left_height, right_height, right);
}

static Real
first_intersection (Building const &b, list<Building> *const s, Real start_x)
{
  while (!s->empty () && start_x < b.end_)
    {
      Building c = s->front ();
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

void
Skyline::internal_merge_skyline (list<Building> *s1, list<Building> *s2,
                                 list<Building> *const result)
{
  if (s1->empty () || s2->empty ())
    {
      programming_error ("tried to merge an empty skyline");
      return;
    }

  Real x = -infinity_f;
  while (!s1->empty ())
    {
      if (s2->front ().conceals (s1->front (), x))
        swap (s1, s2);

      Building b = s1->front ();
      Real end = first_intersection (b, s2, x);

      if (s2->empty ())
        {
          result->push_front (b);
          break;
        }

      /* only include buildings wider than epsilon */
      if (end > x + EPS)
        {
          b.leading_part (end);
          result->push_front (b);
        }

      if (end >= s1->front ().end_)
        s1->pop_front ();

      x = end;
    }
  result->reverse ();
}

static void
empty_skyline (list<Building> *const ret)
{
  ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f));
}

/*
  Given Building 'b' with starting wall location 'start', extend each side
  with a sloped roofline of width 'horizon_padding'; put the skyline in 'ret'
*/
static void
single_skyline (Building b, Real start, Real horizon_padding, list<Building> *const ret)
{
  bool sloped_neighbours = horizon_padding > 0 && !isinf (start) && !isinf (b.end_);
  if (!isinf (b.end_))
    ret->push_front (Building (b.end_ + horizon_padding, -infinity_f,
                               -infinity_f, infinity_f));
  if (sloped_neighbours)
    ret->push_front (b.sloped_neighbour (start, horizon_padding, RIGHT));

  if (b.end_ > start + EPS)
    ret->push_front (b);

  if (sloped_neighbours)
    ret->push_front (b.sloped_neighbour (start, horizon_padding, LEFT));

  if (!isinf (start))
    ret->push_front (Building (-infinity_f, -infinity_f,
                               -infinity_f, start - horizon_padding));
}

/* remove a non-overlapping set of boxes from BOXES and build a skyline
   out of them */
static list<Building>
non_overlapping_skyline (list<Box> *const boxes, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  list<Building> result;
  Real last_end = -infinity_f;
  list<Box>::iterator i = boxes->begin ();
  while (i != boxes->end ())
    {
      Interval iv = (*i)[horizon_axis];

      if (iv[LEFT] - horizon_padding < last_end)
        {
          i++;
          continue;
        }

      if (iv[LEFT] - horizon_padding > last_end + EPS)
        result.push_front (Building (last_end, -infinity_f, -infinity_f, iv[LEFT] - 2 * horizon_padding));

      Building b (*i, horizon_padding, horizon_axis, sky);
      bool sloped_neighbours = horizon_padding > 0 && !isinf (iv.length ());
      if (sloped_neighbours)
        result.push_front (b.sloped_neighbour (iv[LEFT] - horizon_padding, horizon_padding, LEFT));
      result.push_front (b);
      if (sloped_neighbours)
        result.push_front (b.sloped_neighbour (iv[LEFT] - horizon_padding, horizon_padding, RIGHT));

      list<Box>::iterator j = i++;
      boxes->erase (j);
      last_end = result.front ().end_;
    }
  if (last_end < infinity_f)
    result.push_front (Building (last_end, -infinity_f, -infinity_f, infinity_f));
  result.reverse ();
  return result;
}

class LessThanBox
{
  Axis a_;

public:
  LessThanBox (Axis a)
  {
    a_ = a;
  }

  bool operator () (Box const &b1, Box const &b2)
  {
    return b1[a_][LEFT] < b2[a_][LEFT];
  }
};

list<Building>
Skyline::internal_build_skyline (list<Box> *boxes, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  vsize size = boxes->size ();

  if (size == 0)
    {
      list<Building> result;
      empty_skyline (&result);
      return result;
    }
  else if (size == 1)
    {
      list<Building> result;
      single_skyline (Building (boxes->front (), horizon_padding, horizon_axis, sky),
                      boxes->front ()[horizon_axis][LEFT] - horizon_padding,
                      horizon_padding, &result);
      return result;
    }

  deque<list<Building> > partials;
  boxes->sort (LessThanBox (horizon_axis));
  while (!boxes->empty ())
    partials.push_back (non_overlapping_skyline (boxes, horizon_padding, horizon_axis, sky));

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
  build padded skyline from an existing skyline with padding
  added to it.
*/

Skyline::Skyline (Skyline const &src, Real horizon_padding, Axis /*a*/)
{
  /*
     We extract boxes from the skyline, then build a new skyline from
     the boxes.
     A box is created for every horizontal portion of the skyline
     Because skylines are defined positive, and then inverted if they
     are to be down-facing, we create the new skyline in the UP
     direction, then give it the down direction if needed.
  */
  Real start = -infinity_f;
  list<Box> boxes;

  // establish a baseline box
  // FIXME: This has hardcoded logic, assuming a == X_AXIS!
  boxes.push_back (Box (Interval (-infinity_f, infinity_f),
                        Interval (0, 0)));
  list<Building>::const_iterator end = src.buildings_.end ();
  for (list<Building>::const_iterator i = src.buildings_.begin (); i != end; start = i->end_, i++)
    if ((i->slope_ == 0) && !isinf (i->y_intercept_))
      boxes.push_back (Box (Interval (start, i->end_),
                            Interval (-infinity_f, i->y_intercept_)));
  buildings_ = internal_build_skyline (&boxes, horizon_padding, X_AXIS, UP);
  sky_ = src.sky_;
}

/*
  build skyline from a set of boxes. If horizon_padding > 0, expand all the boxes
  by that amount and add 45-degree sloped boxes to the edges of each box (of
  width horizon_padding). That is, the total amount of horizontal expansion is
  horizon_padding*4, half of which is sloped and half of which is flat.

  Boxes should have fatness in the horizon_axis (after they are expanded by
  horizon_padding), otherwise they are ignored.
 */
Skyline::Skyline (vector<Box> const &boxes, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  list<Box> filtered_boxes;
  sky_ = sky;

  Axis vert_axis = other_axis (horizon_axis);
  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][horizon_axis];
      iv.widen (horizon_padding);
      if (iv.length () > EPS && !boxes[i][vert_axis].is_empty ())
        filtered_boxes.push_front (boxes[i]);
    }

  buildings_ = internal_build_skyline (&filtered_boxes, horizon_padding, horizon_axis, sky);
}

Skyline::Skyline (Box const &b, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  sky_ = sky;
  Building front (b, horizon_padding, horizon_axis, sky);
  single_skyline (front, b[horizon_axis][LEFT] - horizon_padding,
                  horizon_padding, &buildings_);
}

void
Skyline::merge (Skyline const &other)
{
  assert (sky_ == other.sky_);

  list<Building> other_bld (other.buildings_);
  list<Building> my_bld;
  my_bld.splice (my_bld.begin (), buildings_);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
}

void
Skyline::insert (Box const &b, Real horizon_padding, Axis a)
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
  iv.widen (horizon_padding);
  if (iv.length () <= EPS || b[other_axis (a)].is_empty ())
    return;

  my_bld.splice (my_bld.begin (), buildings_);
  single_skyline (Building (b, horizon_padding, a, sky_), b[a][LEFT] - horizon_padding,
                  horizon_padding, &other_bld);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
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
  assert (sky_ == -other.sky_);

  Skyline const *padded_this = this;
  Skyline const *padded_other = &other;
  bool created_tmp_skylines = false;

  /*
    For systems, padding is not added at creation time.  Padding is
    added to AxisGroup objects when outside-staff objects are added.
    Thus, when we want to place systems with horizontal padding,
    we do it at distance calculation time.
  */
  if (horizon_padding != 0.0)
    {
      padded_this = new Skyline (*padded_this, horizon_padding, X_AXIS);
      padded_other = new Skyline (*padded_other, horizon_padding, X_AXIS);
      created_tmp_skylines = true;
    }

  list<Building>::const_iterator i = padded_this->buildings_.begin ();
  list<Building>::const_iterator j = padded_other->buildings_.begin ();

  Real dist = -infinity_f;
  Real start = -infinity_f;
  Real touch = -infinity_f;
  while (i != padded_this->buildings_.end () && j != padded_other->buildings_.end ())
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

  if (created_tmp_skylines)
    {
      delete padded_this;
      delete padded_other;
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
  Skyline s (-sky_);
  s.set_minimum_height (0);
  return sky_ * distance (s);
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
Skyline::mark_smob (SCM)
{
  ASSERT_LIVE_IS_ALLOWED ();
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
