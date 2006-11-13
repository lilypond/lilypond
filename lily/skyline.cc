/* skyline.cc -- implement the Skyline class

   source file of the GNU LilyPond music typesetter
 
   (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "skyline.hh"

/* A skyline is a sequence of non-overlapping buildings: something like
   this:
                  _________
                 |         |                                ________
                 |         |                      _________|        |
        /|       |          \                    |                  |
       / |-------             \                  |                  |
      /                         \                |                  |
     /                            ---------------                   -------
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

bool
Skyline::is_legal_skyline () const
{
  list<Building>::const_iterator i;
  Real last_x = -infinity_f;
  for (i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (isinf (i->start_height_) != isinf (i->end_height_))
	return false;
      if (i->iv_[LEFT] != last_x)
	return false;
      if (isinf (i->iv_.length ()) && i->start_height_ != i->end_height_)
	return false;
      last_x = i->iv_[RIGHT];
    }
  return last_x == infinity_f;
}

Building::Building (Real start, Real start_height, Real end_height, Real end)
  : iv_ (start, end)
{
  start_height_ = start_height;
  end_height_ = end_height;

  if (isinf (start_height) || isinf (start) || isinf (end))
    end_height_ = start_height;
  else if (isinf (end_height))
    start_height_ = end_height;

  m_ = (end_height - start_height) / (end - start);
  b_ = start_height - m_*start;

  if (isinf (start_height) || isinf (start) || isinf (end))
    {
      m_ = 0;
      b_ = start_height;
    }
}

Real 
Building::height (Real x) const
{
  if (isinf (x))
    return start_height_;
  return m_*x + b_;
}

Real
Building::intersection (Building const &other) const
{
  return (b_ - other.b_) / (other.m_ - m_);
}

void
Building::leading_part (Real chop)
{
  assert (chop > iv_[LEFT] && chop <= iv_[RIGHT] && !equal (chop, iv_[LEFT]));
  iv_[RIGHT] = chop;
  end_height_ = height (chop);
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
      sky->front ().start_height_ = sky->front ().height (x);
    }
}

bool
Building::obstructs (Building const &other) const
{
  if (equal (intersection (other), iv_[LEFT]) || equal (start_height_, other.start_height_))
    return m_ > other.m_;
  return start_height_ > other.start_height_;
}


/* precondition: the building should be visible above the first
   building in skyline. The building and the skyline should
   start at the same point.

   return the point at which the building b is no longer visible,
   either because it has ended or because the skyline has risen
   above it. Truncate the skyline at that point.
*/
Real
Skyline::last_visible_point (Building const &b, list<Building> *const skyline)
{
  assert (!skyline->front ().obstructs (b));
  while (1)
    {
      Building other = skyline->front ();

      /* there are 3 interesting cases:
	 1) the roofs intersect within the spans of the buildings */
      Real intersect = b.intersection (other);
      if (intersection (b.iv_, other.iv_).contains (intersect))
	{
	  if (equal (intersect, b.iv_[LEFT]))
	    {
	      /* if the buildings have almost the same starting height, we can find
		 that their intersection "equals" the start point. In this case, we
		 just skip the intersection.
	      */
	      assert (b.m_ >= other.m_);
	    }
	  else
	    {
	      skyline_trailing_part (skyline, intersect);
	      return intersect;
	    }
	}

      /* 2) the first building ends. This is guaranteed to happen before
            the skyline becomes empty because it has to end at infinity */
      if (skyline->empty () && !other.iv_.contains (b.iv_[RIGHT]))
	assert (0);
      if (other.iv_.contains (b.iv_[RIGHT]))
	{
	  skyline_trailing_part (skyline, b.iv_[RIGHT]);
	  return b.iv_[RIGHT];
	}

      assert (!skyline->empty ());
      skyline->pop_front ();
      other = skyline->front ();

      /* 3) the next building in the skyline starts above b */
      if (other.start_height_ > b.height (other.iv_[LEFT]))
	return other.iv_[LEFT];
    }
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
      Real end = last_visible_point (b, s2);

      b.leading_part (end);
      result->push_front (b);

      skyline_trailing_part (s1, end);
    }
  result->reverse ();
}

static void
empty_skyline (list<Building> *const ret)
{
  ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, infinity_f));
}

static void
single_skyline (Building const &b, list<Building> *const ret)
{
  if (!isinf (b.iv_[RIGHT]))
    ret->push_front (Building (b.iv_[RIGHT], -infinity_f, -infinity_f, infinity_f));
  ret->push_front (b);
  if (!isinf (b.iv_[LEFT]))
    ret->push_front (Building (-infinity_f, -infinity_f, -infinity_f, b.iv_[LEFT]));
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
      single_skyline (buildings->front (), result);
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

Skyline::Skyline (Direction sky)
{
  sky_ = sky;
  empty_skyline (&buildings_);
}

Skyline::Skyline (vector<Box> const &boxes, Axis a, Direction sky)
{
  list<Building> bldgs;
  sky_ = sky;

  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][a];
      Real height = sky * boxes[i][other_axis (a)][sky];
      if (!iv.is_empty () && !isinf (height) && !equal (iv[LEFT], iv[RIGHT]))
	bldgs.push_front (Building (iv[LEFT], height, height, iv[RIGHT]));
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
Skyline::insert (Box const &b, Axis a)
{
  list<Building> other_bld;
  list<Building> my_bld (buildings_);
  Interval iv = b[a];
  Real height = sky_ * b[other_axis (a)][sky_];

  single_skyline (Building (iv[LEFT], height, height, iv[RIGHT]), &other_bld);
  internal_merge_skyline (&other_bld, &my_bld, &buildings_);
  assert (is_legal_skyline ());
}

void
Skyline::raise (Real r)
{
  list<Building>::iterator end = buildings_.end ();
  for (list<Building>::iterator i = buildings_.begin (); i != end; i++)
    {
      i->start_height_ += sky_ * r;
      i->end_height_ += sky_ * r;
      i->b_ += sky_ * r;
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
  for (; i != buildings_.end () && j != other.buildings_.end (); i++)
    {
      while (j->iv_[RIGHT] < i->iv_[LEFT])
	j++;

      list<Building>::const_iterator k;
      for (k = j; k->iv_[LEFT] <= i->iv_[RIGHT] && k != other.buildings_.end (); k++)
	{
	  Interval iv = intersection (i->iv_, k->iv_);
	  dist = max (dist, max (i->height (iv[LEFT]) + k->height (iv[LEFT]),
				 i->height (iv[RIGHT]) + k->height (iv[RIGHT])));
	}
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
      if (i->iv_[RIGHT] > airplane)
	return sky_ * i->height (airplane);
      if (i->iv_[RIGHT] == airplane)
	{
	  assert (i != buildings_.end ());
	  list<Building>::const_iterator j = i;
	  j++;
	  return sky_ * (max (i->end_height_, j->start_height_));
	}
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
  s.buildings_.front ().start_height_ = h*sky_;
  s.buildings_.front ().end_height_ = h*sky_;
  s.buildings_.front ().b_ = h*sky_;
  merge (s);
}
