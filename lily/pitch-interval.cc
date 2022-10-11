/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "pitch-interval.hh"

#include "interval.tcc"

Pitch_interval::Pitch_interval (Pitch p1, Pitch p2)
{
  at (LEFT) = p1;
  at (RIGHT) = p2;
}

Pitch_interval::Pitch_interval ()
{
  at (LEFT) = Pitch (100, 0);
  at (RIGHT) = Pitch (-100, 0);
}

bool
Pitch_interval::is_empty () const
{
  return at (LEFT) > at (RIGHT);
}

Drul_array<bool>
Pitch_interval::add_point (Pitch p)
{
  Drul_array<bool> expansions;
  if (at (LEFT).tone_pitch () > p.tone_pitch ())
    {
      at (LEFT) = p;
      expansions[LEFT] = true;
    }
  if (at (RIGHT).tone_pitch () < p.tone_pitch ())
    {
      at (RIGHT) = p;
      expansions[RIGHT] = true;
    }
  return expansions;
}

Pitch_lexicographic_interval::Pitch_lexicographic_interval (Pitch p1, Pitch p2)
{
  at (LEFT) = p1;
  at (RIGHT) = p2;
}

Pitch_lexicographic_interval::Pitch_lexicographic_interval ()
{
  at (LEFT) = Pitch (100, 0);
  at (RIGHT) = Pitch (-100, 0);
}

bool
Pitch_lexicographic_interval::is_empty () const
{
  return at (LEFT) > at (RIGHT);
}

Drul_array<bool>
Pitch_lexicographic_interval::add_point (Pitch p)
{
  Drul_array<bool> expansions;
  if (at (LEFT) > p)
    {
      at (LEFT) = p;
      expansions[LEFT] = true;
    }
  if (at (RIGHT) < p)
    {
      at (RIGHT) = p;
      expansions[RIGHT] = true;
    }
  return expansions;
}
