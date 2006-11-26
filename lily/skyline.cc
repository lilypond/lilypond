/* skyline.cc -- implement the Skyline class

   source file of the GNU LilyPond music typesetter
 
   (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "skyline.hh"

#include "line-interface.hh"

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
      if (i != buildings_.begin () && !equal (i->start_height_, last_h))
	return false;
      last_x = i->iv_[RIGHT];
      last_h = i->end_height_;
    }
  return last_x == infinity_f;
}

Building::Building (Real start, Real start_height, Real end_height, Real end, Real max_slope)
  : iv_ (start, end)
{
  start_height_ = start_height;
  end_height_ = end_height;

  if (isinf (start))
    assert (isinf (start_height) || start_height == end_height);
  if (isinf (end))
    assert (isinf (end_height) || start_height == end_height);

  m_ = (end_height - start_height) / (end - start);
  if (start_height == end_height)
    m_ = 0;
  if (isinf (m_) || isnan (m_))
    m_ = max_slope * (start_height < end_height ? 1 : -1);
  assert (abs (m_) <= max_slope);

  if (isinf (start))
    {
      if (isinf (end))
	b_ = start_height;
      else
	b_ = end_height - m_*end;
    }
  else
    b_ = start_height - m_*start;
}

Real 
Building::height (Real x) const
{
  if (isinf (x))
    return (x > 0) ? end_height_ : start_height_;
  return m_*x + b_;
}

Real
Building::intersection (Building const &other) const
{
  return (b_ - other.b_) / (other.m_ - m_);
}

void
Building::leading_part (Real chop, Real h)
{
  assert (chop > iv_[LEFT] && chop <= iv_[RIGHT] && !equal (chop, iv_[LEFT]));
  assert (equal (h, height (chop)));
  iv_[RIGHT] = chop;
  end_height_ = h;
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
    return m_ > other.m_ || (m_ == other.m_ && b_ > other.b_);
  return start_height_ > other.start_height_;
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
	     && s2->front ().end_height_ <= b.height (s2->front ().iv_[RIGHT]) + EPS)
	s2->pop_front ();

      /* the front of s2 either intersects with b or it ends after b */
      Real end = infinity_f;
      Real s2_end_height = s2->front ().end_height_;
      Real s1_end_height = b.height (s2->front ().iv_[RIGHT]);
      if (s2_end_height > s1_end_height + EPS)
	end = b.intersection (s2->front ());
      end = min (end, b.iv_[RIGHT]);
      Real height = b.height (end);

      b.leading_part (end, height);
      result->push_front (b);

      skyline_trailing_part (s1, end);
      if (!s1->empty ())
	s1->front ().start_height_ = height;
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
    ret->push_front (Building (b.iv_[RIGHT], b.end_height_, -infinity_f, infinity_f, max_slope));
  ret->push_front (b);
  if (!isinf (b.iv_[LEFT]))
    ret->push_front (Building (-infinity_f, -infinity_f, b.start_height_, b.iv_[LEFT], max_slope));
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
      single_skyline (buildings->front (), result, max_slope_);
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
  max_slope_ = 2;
  sky_ = UP;
  empty_skyline (&buildings_);
}

Skyline::Skyline (Direction sky)
{
  max_slope_ = 2;
  sky_ = sky;
  empty_skyline (&buildings_);
}

Skyline::Skyline (vector<Box> const &boxes, Axis a, Direction sky)
{
  list<Building> bldgs;
  sky_ = sky;
  max_slope_ = 2;

  for (vsize i = 0; i < boxes.size (); i++)
    {
      Interval iv = boxes[i][a];
      Real height = sky * boxes[i][other_axis (a)][sky];
      if (!iv.is_empty () && !isinf (height) && !equal (iv[LEFT], iv[RIGHT]))
	bldgs.push_front (Building (iv[LEFT], height, height, iv[RIGHT], max_slope_));
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
  list<Building> my_bld;
  Interval iv = b[a];
  Real height = sky_ * b[other_axis (a)][sky_];

  my_bld.splice (my_bld.begin (), buildings_);
  single_skyline (Building (iv[LEFT], height, height, iv[RIGHT], max_slope_), &other_bld, max_slope_);
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
  s.buildings_.front ().start_height_ = h*sky_;
  s.buildings_.front ().end_height_ = h*sky_;
  s.buildings_.front ().b_ = h*sky_;
  merge (s);
}

Stencil
Skyline::stencil ()
{
  Stencil ret;
  for (list<Building>::iterator i = buildings_.begin (); i != buildings_.end (); i++)
    {
      if (!isinf (i->iv_.length ()))
	{
	  Stencil line = Line_interface::make_line (0.1,
						    Offset (i->iv_[LEFT], sky_*i->start_height_),
						    Offset (i->iv_[RIGHT], sky_*i->end_height_));
	  ret.add_stencil (line);
	}
    }
  return ret;
}
