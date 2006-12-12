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
approx_equal (Real x, Real y)
{
  return abs (x - y) < EPS || (isinf (x) && isinf (y) && ((x > 0) == (y > 0)));
}

static inline bool
approx_greater_than (Real x, Real y)
{
  return x > y + EPS;
}

static inline bool
approx_less_than (Real x, Real y)
{
  return x < y - EPS;
}

static inline bool
approx_less_equal (Real x, Real y)
{
  return x <= y + EPS;
}

static inline bool
approx_greater_equal (Real x, Real y)
{
  return x >= y - EPS;
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
  for (i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (i->iv_[LEFT] != last_x)
	return false;
      last_x = i->iv_[RIGHT];
      if (isinf (i->iv_.length ()) && i->height_[LEFT] != i->height_[RIGHT])
	return false;
    }
  return last_x == infinity_f;
}

Building::Building (Real start, Real start_height, Real end_height, Real end)
  : iv_ (start, end)
{
  height_[LEFT] = start_height;
  height_[RIGHT] = end_height;

  if (isinf (start) || isinf (end))
    assert (start_height == end_height);

  precompute ();
}

void
Building::precompute ()
{
  slope_ = (height_[RIGHT] - height_[LEFT]) / (iv_.length());
  if (height_[LEFT] == height_[RIGHT]) /* in case they're both infinity */
    slope_ = 0;

  assert (!isinf (slope_) && !isnan (slope_));

  if (isinf (iv_[START]))
    {
      assert (slope_ == 0);
      y_intercept_ = height_[LEFT];
    }
  else
    y_intercept_ = height_[LEFT] - slope_ * iv_[START];
}

Real 
Building::height (Real x) const
{
  if (isinf (x))
    return (x > 0) ? height_[RIGHT] : height_[LEFT];
  return slope_*x + y_intercept_;
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
  return (y_intercept_ - other.y_intercept_) / (other.slope_ - slope_);
}

void
Building::leading_part (Real chop)
{
  assert (chop > iv_[LEFT] && chop <= iv_[RIGHT] && !approx_equal (chop, iv_[LEFT]));
  iv_[RIGHT] = chop;
  height_[RIGHT] = height (chop);
}

Building
Building::sloped_neighbour (Real horizon_padding, Direction d) const
{
  Real left = iv_[d];
  Real right = iv_[d] + d * horizon_padding;
  Real left_height = height_[d];
  Real right_height = height_[d] - horizon_padding;
  if (d == LEFT)
    {
      swap (left, right);
      swap (left_height, right_height);
    }
  return Building (left, left_height, right_height, right);
}

static void
skyline_trailing_part (list<Building> *sky, Real x)
{
  if (approx_equal (x, sky->front ().iv_[RIGHT]))
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
Building::conceals_beginning (Building const &other) const
{
  if (approx_equal (intersection (other), iv_[LEFT]) || approx_equal (height_[LEFT], other.height_[LEFT]))
    return slope_ > other.slope_;
  return height_[LEFT] > other.height_[LEFT];
}

bool
Building::conceals (Building const &other) const
{
  assert (iv_[LEFT] <= other.iv_[LEFT]);
  return (iv_[RIGHT] >= other.iv_[RIGHT])
    && approx_greater_equal (height (other.iv_[LEFT]), other.height_[LEFT])
    && approx_greater_equal (height (other.iv_[RIGHT]), other.height_[RIGHT]);
}

void
Skyline::internal_merge_skyline (list<Building> *s1, list<Building> *s2,
				 list<Building> *const result)
{
  while (!s1->empty ())
    {
      if (s2->front ().conceals_beginning (s1->front ()))
	swap (s1, s2);

      Building b = s1->front ();
      while (!s2->empty () && b.conceals (s2->front ()))
	s2->pop_front ();
      if (s2->empty ())
	{
	  result->push_front (b);
	  break;
	}

      /* s2 either intersects with b or it ends after b */
      Real end = infinity_f;
      Real s2_start_height = s2->front ().height_[LEFT];
      Real s2_end_height = s2->front ().height_[RIGHT];
      Real s1_start_height = b.height (s2->front ().iv_[LEFT]);
      Real s1_end_height = b.height (s2->front ().iv_[RIGHT]);
      if (approx_greater_than (s2_start_height, s1_start_height))
	end = s2->front ().iv_[LEFT];
      else if (approx_greater_than (s2_end_height, s1_end_height))
	end = b.intersection (s2->front ());
      end = min (end, b.iv_[RIGHT]);

      b.leading_part (end);
      result->push_front (b);

      skyline_trailing_part (s1, end);
      skyline_trailing_part (s2, end);
    }
  result->reverse ();
}

static void
empty_skyline (list<Building> *const ret)
{
  ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f));
}

static void
single_skyline (Building b, Real horizon_padding, list<Building> *const ret)
{
  b.iv_.widen (horizon_padding);
  
  if (!isinf (b.iv_[RIGHT]))
    ret->push_front (Building (b.iv_[RIGHT], -infinity_f,
			       -infinity_f, infinity_f));
  if (horizon_padding > 0)
    ret->push_front (b.sloped_neighbour (horizon_padding, RIGHT));
  
  if (b.iv_[RIGHT] > b.iv_[LEFT])
    ret->push_front (b);

  if (horizon_padding > 0)
    ret->push_front (b.sloped_neighbour (horizon_padding, LEFT));
  if (!isinf (b.iv_[LEFT]))
    ret->push_front (Building (-infinity_f, -infinity_f,
			       -infinity_f, b.iv_[LEFT]));
}

void
Skyline::internal_build_skyline (list<Building> *buildings, list<Building> *const result)
{
  vsize size = buildings->size ();

  if (size == 0)
    {
      empty_skyline (result);
      return;
    }
  else if (size == 1)
    {
      single_skyline (buildings->front (), 0, result);
      return;
    }

  list<Building> right_half;
  list<Building>::iterator i = buildings->begin ();

  for (vsize s = 0; s < size/2; s++)
    i++;
  right_half.splice (right_half.end (), *buildings, i, buildings->end ());

  list<Building> right;
  list<Building> left;
  internal_build_skyline (&right_half, &right);
  internal_build_skyline (buildings, &left);
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
  build skyline from a set of boxes. If horizon_padding > 0, expand all the boxes
  by that amount and add 45-degree sloped boxes to the edges of each box (of
  width horizon_padding). That is, the total amount of horizontal expansion is
  horizon_padding*4, half of which is sloped and half of which is flat.

  Boxes should have fatness in the horizon_axis (after they are expanded by
  horizon_padding), otherwise they are ignored.
 */
Skyline::Skyline (vector<Box> const &boxes, Real horizon_padding, Axis horizon_axis, Direction sky)
{
  list<Building> bldgs;
  sky_ = sky;

  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][horizon_axis];
      Real height = sky * boxes[i][other_axis (horizon_axis)][sky];
      
      iv.widen (horizon_padding);
      if (!iv.is_empty () && !isinf (height) && !approx_equal (iv[LEFT], iv[RIGHT]))
	{
	  iv.widen (EPS);
	  Building front = Building (iv[LEFT], height, height, iv[RIGHT]);
	  bldgs.push_front (front);
	  if (horizon_padding > 0)
	    {
	      bldgs.push_front (front.sloped_neighbour (horizon_padding, LEFT));
	      bldgs.push_front (front.sloped_neighbour (horizon_padding, RIGHT));
	    }
	}
    }
  
  internal_build_skyline (&bldgs, &buildings_);
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
Skyline::insert (Box const &b, Real horizon_padding, Axis a)
{
  list<Building> other_bld;
  list<Building> my_bld;
  Interval iv = b[a];
  Real height = sky_ * b[other_axis (a)][sky_];

  assert (!iv.is_empty ());
  iv.widen (EPS);

  my_bld.splice (my_bld.begin (), buildings_);
  single_skyline (Building (iv[LEFT], height, height, iv[RIGHT]), horizon_padding, &other_bld);
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
      i->y_intercept_ += sky_ * r;
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
  s.buildings_.front ().y_intercept_ = h * sky_;
  merge (s);
}


vector<Offset>
Skyline::to_points () const
{
  vector<Offset> out;

  for (list<Building>::const_iterator i (buildings_.begin ());
       i != buildings_.end (); i++)
    {
      if (!isinf (i->iv_[LEFT]) && !isinf (i->height_[LEFT]))
	out.push_back (Offset (i->iv_[LEFT], sky_ * i->height_[LEFT]));
      if (!isinf (i->iv_[RIGHT]) && !isinf (i->height_[RIGHT]))
	out.push_back (Offset (i->iv_[RIGHT], sky_ * i->height_[RIGHT]));
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
