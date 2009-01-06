/* skyline.cc -- implement the Skyline class

   source file of the GNU LilyPond music typesetter
 
   (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
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
    printf ("(%f,%f)%s" , ps[i][X_AXIS], ps[i][Y_AXIS],
	    (i%2)==1 ? "\n" : " ");
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
  return isinf (x) ? y_intercept_ : slope_*x + y_intercept_;
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
	result.push_front (Building (last_end, -infinity_f, -infinity_f, iv[LEFT] - 2*horizon_padding));

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

  bool operator() (Box const &b1, Box const &b2)
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
		      boxes->front ()[horizon_axis][LEFT], horizon_axis, &result);
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
  single_skyline (front, b[horizon_axis][LEFT], horizon_padding, &buildings_);
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
  single_skyline (Building (b, horizon_padding, a, sky_), b[a][LEFT], horizon_padding, &other_bld);
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
Skyline::distance (Skyline const &other) const
{
  assert (sky_ == -other.sky_);
  list<Building>::const_iterator i = buildings_.begin ();
  list<Building>::const_iterator j = other.buildings_.begin ();

  Real dist = -infinity_f;
  Real start = -infinity_f;
  while (i != buildings_.end () && j != other.buildings_.end ())
    {
      Real end = min (i->end_, j->end_);
      Real start_dist = i->height (start) + j->height (start);
      Real end_dist = i->height (end) + j->height (end);
      dist = max (dist, max (start_dist, end_dist));
      if (i->end_ <= j->end_)
	i++;
      else
	j++;
      start = end;
    }
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
