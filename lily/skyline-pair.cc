/* 
  skyline-pair.cc -- implement Skyline_pair
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "skyline-pair.hh"

#include "ly-smobs.icc"

Skyline_pair::Skyline_pair ()
  : skylines_ (Skyline (DOWN), Skyline (UP))
{
}

Skyline_pair::Skyline_pair (vector<Box> const &boxes, Real padding, Axis a)
  : skylines_ (Skyline (boxes, padding, a, DOWN), Skyline (boxes, padding, a, UP))
{
}

Skyline_pair::Skyline_pair (Box const &b, Real padding, Axis a)
  : skylines_ (Skyline (b, padding, a, DOWN), Skyline (b, padding, a, UP))
{
}

void
Skyline_pair::raise (Real r)
{
  skylines_[UP].raise (r);
  skylines_[DOWN].raise (r);
}

void
Skyline_pair::shift (Real r)
{
  skylines_[UP].shift (r);
  skylines_[DOWN].shift (r);
}

void
Skyline_pair::insert (Box const &b, Real padding, Axis a)
{
  skylines_[UP].insert (b, padding, a);
  skylines_[DOWN].insert (b, padding, a);
}

void
Skyline_pair::merge (Skyline_pair const &other)
{
  skylines_[UP].merge (other[UP]);
  skylines_[DOWN].merge (other[DOWN]);
}

void
Skyline_pair::print () const
{
  skylines_[UP].print ();
  skylines_[DOWN].print ();
}

void
Skyline_pair::print_points () const
{
  skylines_[UP].print ();
  skylines_[DOWN].print ();
}


bool
Skyline_pair::is_empty () const
{
  return skylines_[UP].is_empty ()
    && skylines_[DOWN].is_empty ();
}

Skyline&
Skyline_pair::operator [] (Direction d)
{
  return skylines_[d];
}

Skyline const&
Skyline_pair::operator [] (Direction d) const
{
  return skylines_[d];
}

IMPLEMENT_SIMPLE_SMOBS (Skyline_pair);
IMPLEMENT_TYPE_P (Skyline_pair, "ly:skyline-pair?");
IMPLEMENT_DEFAULT_EQUAL_P (Skyline_pair);

SCM
Skyline_pair::mark_smob (SCM)
{
  return SCM_EOL;
}

int
Skyline_pair::print_smob (SCM s, SCM port, scm_print_state *)
{
  Skyline_pair *r = (Skyline_pair *) SCM_CELL_WORD_1 (s);
  (void) r;

  scm_puts ("#<Skyline-pair>", port);
  return 1;
}
