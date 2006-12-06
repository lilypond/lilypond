/* skyline.cc -- implement the Skyline class

   source file of the GNU LilyPond music typesetter
 
   (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "skyline.hh"

#include "ly-smobs.icc"

/* A skyline is a sequence of non-overlapping buildings: something like
   this:
                   _______
                  /       \                                ________
                 /         \                      ________/        \
        /\      /           \                    /                  \
       /  -----/              \                 /                    \
      /                         \              /                      \
     /                            ------------/                        ----
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

#define EPS 1e-10

static inline bool
equal (Real x, Real y)
{
  return abs (x - y) < EPS || (isinf (x) && isinf (y) && ((x > 0) == (y > 0)));
}

void
Skyline::print () const
{
  for (list<Building>::const_iterator i = buildings_.begin ();
       i != buildings_.end (); i++)
    {
      (*i).print ();
    }
}

bool
Skyline::is_legal_skyline () const
{
  list<Building>::const_iterator i;
  Real last_x = -infinity_f;
  Real last_h = -infinity_f;
  for (i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (i->iv_[LEFT] != last_x)
	return false;
      if (i != buildings_.begin () && !equal (i->height_[LEFT], last_h))
	return false;
      last_x = i->iv_[RIGHT];
      last_h = i->height_[RIGHT];
    }
  return last_x == infinity_f;
}

Building::Building (Real start, Real start_height, Real end_height,
		    Real end, Real max_slope)
  : iv_ (start, end)
{
  height_[LEFT] = start_height;
  height_[RIGHT] = end_height;

  if (isinf (start))
    assert (isinf (start_height) || start_height == end_height);
  if (isinf (end))
    assert (isinf (end_height) || start_height == end_height);

  precompute (max_slope);
}

void
Building::precompute (Real max_slope)
{
  slope_ = (height_[RIGHT] - height_[LEFT]) / (iv_.length());
  if (height_[LEFT] == height_[RIGHT])
    slope_ = 0;
  if (isinf (slope_) || isnan (slope_))
    slope_ = max_slope * (height_[LEFT] < height_[RIGHT] ? 1 : -1);

#if 0
  /*
    this check is sensitive to roundoff errors when converting to/from
    sequences of points.
   */
  assert (abs (slope_) <= max_slope + EPS);
#endif
  
  if (isinf (iv_[START]))
    {
      if (isinf (iv_[STOP]))
	zero_height_ = height_[LEFT];
      else
	zero_height_ = height_[RIGHT] - slope_ * iv_[STOP];
    }
  else
    zero_height_ = height_[LEFT] - slope_ * iv_[START];
}

Real 
Building::height (Real x) const
{
  if (isinf (x))
    return (x > 0) ? height_[RIGHT] : height_[LEFT];
  return slope_*x + zero_height_;
}

void
Building::print () const
{
  printf ("X[%f,%f] -> Y[%f,%f]\n",
	  iv_[LEFT], iv_[RIGHT],
	  height_[LEFT], height_[RIGHT]);
}

Real
Building::intersection (Building const &other) const
{
  return (zero_height_ - other.zero_height_) / (other.slope_ - slope_);
}

void
Building::leading_part (Real chop, Real h)
{
  assert (chop > iv_[LEFT] && chop <= iv_[RIGHT] && !equal (chop, iv_[LEFT]));
  assert (equal (h, height (chop)));
  iv_[RIGHT] = chop;
  height_[RIGHT] = h;
}

static void
skyline_trailing_part (list<Building> *sky, Real x)
{
  if (equal (x, sky->front ().iv_[RIGHT]))
    sky->pop_front ();
  else
    assert (x < sky->front ().iv_[RIGHT]);

  if (!sky->empty ())
    {
      sky->front ().iv_[LEFT] = x;
      sky->front ().height_[LEFT] = sky->front ().height (x);
    }
}

bool
Building::obstructs (Building const &other) const
{
  if (equal (intersection (other), iv_[LEFT]) || equal (height_[LEFT], other.height_[LEFT]))
    return slope_ > other.slope_ || (slope_ == other.slope_ && zero_height_ > other.zero_height_);
  return height_[LEFT] > other.height_[LEFT];
}

void
Skyline::internal_merge_skyline (list<Building> *s1, list<Building> *s2,
				 list<Building> *const result)
{
  while (!s1->empty ())
    {
      if (s2->front ().obstructs (s1->front ()))
	swap (s1, s2);

      Building b = s1->front ();
      while (s2->front ().iv_[RIGHT] < b.iv_[RIGHT]
	     && s2->front ().height_[RIGHT] <= b.height (s2->front ().iv_[RIGHT]) + EPS)
	s2->pop_front ();

      /* the front of s2 either intersects with b or it ends after b */
      Real end = infinity_f;
      Real s2_end_height = s2->front ().height_[RIGHT];
      Real s1_end_height = b.height (s2->front ().iv_[RIGHT]);
      if (s2_end_height > s1_end_height + EPS)
	end = b.intersection (s2->front ());
      end = min (end, b.iv_[RIGHT]);
      Real height = b.height (end);

      b.leading_part (end, height);
      result->push_front (b);

      skyline_trailing_part (s1, end);
      if (!s1->empty ())
	s1->front ().height_[LEFT] = height;
      skyline_trailing_part (s2, end);
    }
  result->reverse ();
}

static void
empty_skyline (list<Building> *const ret)
{
  ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f, 0));
}

static void
single_skyline (Building const &b, list<Building> *const ret, Real max_slope)
{
  if (!isinf (b.iv_[RIGHT]))
    ret->push_front (Building (b.iv_[RIGHT], b.height_[RIGHT],
			       -infinity_f, infinity_f, max_slope));
  if (b.iv_[RIGHT] > b.iv_[LEFT])
    ret->push_front (b);
  if (!isinf (b.iv_[LEFT]))
    ret->push_front (Building (-infinity_f, -infinity_f,
			       b.height_[LEFT], b.iv_[LEFT], max_slope));
}

void
Skyline::internal_build_skyline (list<Building> *buildings, Real max_slope, list<Building> *const result)
{
  vsize size = buildings->size ();

  if (size == 0)
    {
      empty_skyline (result);
      return;
    }
  else if (size == 1)
    {
      single_skyline (buildings->front (), result, max_slope);
      return;
    }

  list<Building> right_half;
  list<Building>::iterator i = buildings->begin ();

  for (vsize s = 0; s < size/2; s++)
    i++;
  right_half.splice (right_half.end (), *buildings, i, buildings->end ());

  list<Building> right;
  list<Building> left;
  internal_build_skyline (&right_half, max_slope, &right);
  internal_build_skyline (buildings, max_slope, &left);
  internal_merge_skyline (&right, &left, result);
}

Skyline::Skyline ()
{
  sky_ = UP;
  empty_skyline (&buildings_);  
}

Skyline::Skyline (Skyline const &src)
{
  sky_ = src.sky_;
 
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
  build skyline from a set of boxes.

  Boxes should have fatness in the horizon_axis, otherwise they are ignored.
 */
Skyline::Skyline (vector<Box> const &boxes, Real max_slope, Axis horizon_axis, Direction sky)
{
  list<Building> bldgs;
  sky_ = sky;

  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][horizon_axis];
      Real height = sky * boxes[i][other_axis (horizon_axis)][sky];
      if (!iv.is_empty () && !isinf (height) && !equal (iv[LEFT], iv[RIGHT]))
	bldgs.push_front (Building (iv[LEFT], height, height, iv[RIGHT],
				    max_slope));
    }
  
  internal_build_skyline (&bldgs, max_slope, &buildings_);
  assert (is_legal_skyline ());
}

Skyline::Skyline (vector<Offset> const &points, Real max_slope, Direction sky)
{
  sky_ = sky;

  for (vsize i = 1; i < points.size (); i++)
    {
      buildings_.push_back (Building (points[i-1][X_AXIS],
				      sky * points[i-1][Y_AXIS],
				      sky * points[i][Y_AXIS],
				      points[i][X_AXIS], 
				      max_slope));

    }

  assert (is_legal_skyline ());
}

void
Skyline::merge (Skyline const &other)
{
  assert (sky_ == other.sky_);

  list<Building> other_bld (other.buildings_);
  list<Building> my_bld;
  my_bld.splice (my_bld.begin (), buildings_);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
  assert (is_legal_skyline ());
}

void
Skyline::insert (Box const &b, Real max_slope, Axis a)
{
  list<Building> other_bld;
  list<Building> my_bld;
  Interval iv = b[a];
  Real height = sky_ * b[other_axis (a)][sky_];

  assert (!iv.is_empty ());

  my_bld.splice (my_bld.begin (), buildings_);
  single_skyline (Building (iv[LEFT], height, height, iv[RIGHT], max_slope), &other_bld, max_slope);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
  assert (is_legal_skyline ());
}

void
Skyline::raise (Real r)
{
  list<Building>::iterator end = buildings_.end ();
  for (list<Building>::iterator i = buildings_.begin (); i != end; i++)
    {
      i->height_[LEFT] += sky_ * r;
      i->height_[RIGHT] += sky_ * r;
      i->zero_height_ += sky_ * r;
    }
  assert (is_legal_skyline ());
}

Real
Skyline::distance (Skyline const &other) const
{
  assert (sky_ == -other.sky_);
  list<Building>::const_iterator i = buildings_.begin ();
  list<Building>::const_iterator j = other.buildings_.begin ();

  Real dist = -infinity_f;
  while (i != buildings_.end () && j != other.buildings_.end ())
    {
      Interval iv = intersection (i->iv_, j->iv_);
      dist = max (dist, max (i->height (iv[LEFT]) + j->height (iv[LEFT]),
			     i->height (iv[RIGHT]) + j->height (iv[RIGHT])));
      if (i->iv_[RIGHT] <= j->iv_[RIGHT])
	i++;
      else
	j++;
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
      if (i->iv_[RIGHT] >= airplane)
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
  s.buildings_.front ().height_[LEFT] = h * sky_;
  s.buildings_.front ().height_[RIGHT] = h * sky_;
  s.buildings_.front ().zero_height_ = h * sky_;
  merge (s);
}


vector<Offset>
Skyline::to_points () const
{
  vector<Offset> out;

  bool first = true;
  for (list<Building>::const_iterator i (buildings_.begin ());
       i != buildings_.end (); i++)
    {
      if (first)
	out.push_back (Offset ((*i).iv_[LEFT], sky_ * (*i).height_[LEFT]));

      first = false;
      out.push_back (Offset ((*i).iv_[RIGHT], sky_ * (*i).height_[RIGHT]));
    }

  return out;
}

/****************************************************************/


IMPLEMENT_SIMPLE_SMOBS (Skyline);
IMPLEMENT_TYPE_P (Skyline, "ly:skyline?");
IMPLEMENT_DEFAULT_EQUAL_P (Skyline);

SCM
Skyline::mark_smob (SCM)
{
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
