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

#ifndef PITCH_INTERVAL_HH
#define PITCH_INTERVAL_HH

#include "pitch.hh"

class Pitch_interval : public Drul_array<Pitch>
{
public:
  Pitch_interval ();
  Pitch_interval (Pitch, Pitch);
  Drul_array<bool> add_point (Pitch);
  bool is_empty () const;
};

class Pitch_lexicographic_interval : public Drul_array<Pitch>
{
public:
  Pitch_lexicographic_interval ();
  Pitch_lexicographic_interval (Pitch, Pitch);
  Drul_array<bool> add_point (Pitch);
  bool is_empty () const;
};

#endif /* PITCH_INTERVAL_HH */
