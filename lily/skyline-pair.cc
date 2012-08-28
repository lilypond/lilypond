/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2008--2012 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "skyline-pair.hh"

#include "international.hh"
#include "ly-smobs.icc"

Skyline_pair::Skyline_pair ()
  : skylines_ (Skyline (DOWN), Skyline (UP))
{
}

Skyline_pair::Skyline_pair (vector<Box> const &boxes, Axis a)
  : skylines_ (Skyline (boxes, a, DOWN), Skyline (boxes, a, UP))
{
}

Skyline_pair::Skyline_pair (vector<Drul_array<Offset> > const &buildings, Axis a)
  : skylines_ (Skyline (buildings, a, DOWN), Skyline (buildings, a, UP))
{
}

Skyline_pair::Skyline_pair (vector<Skyline_pair> const &skypairs)
  : skylines_ (Skyline (skypairs, DOWN), Skyline (skypairs, UP))
{
}

Skyline_pair::Skyline_pair (Box const &b, Axis a)
  : skylines_ (Skyline (b, a, DOWN), Skyline (b, a, UP))
{
}

void
Skyline_pair::raise (Real r)
{
  skylines_[UP].raise (r);
  skylines_[DOWN].raise (r);
}

void
Skyline_pair::deholify ()
{
  skylines_[UP].deholify ();
  skylines_[DOWN].deholify ();
}

void
Skyline_pair::shift (Real r)
{
  skylines_[UP].shift (r);
  skylines_[DOWN].shift (r);
}

void
Skyline_pair::insert (Box const &b, Axis a)
{
  skylines_[UP].insert (b, a);
  skylines_[DOWN].insert (b, a);
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
  skylines_[UP].print_points ();
  skylines_[DOWN].print_points ();
}

bool
Skyline_pair::is_empty () const
{
  return skylines_[UP].is_empty ()
         && skylines_[DOWN].is_empty ();
}

Skyline &
Skyline_pair::operator [] (Direction d)
{
  return skylines_[d];
}

Skyline const &
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

MAKE_SCHEME_CALLBACK (Skyline_pair, skyline, 2);
SCM
Skyline_pair::skyline (SCM smob, SCM dir_scm)
{
  Skyline_pair *sp = Skyline_pair::unsmob (smob);
  Direction dir = robust_scm2dir (dir_scm, UP);

  if (dir == CENTER)
    {
      warning (_f ("direction must not be CENTER in ly:skyline-pair::skyline"));
      dir = UP;
    }

  return (*sp)[dir].smobbed_copy ();
}
