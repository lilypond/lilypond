/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DOT_CONFIGURATION_HH
#define DOT_CONFIGURATION_HH

#include "lily-proto.hh"
#include "direction.hh"
#include "box.hh"

#include <map>

struct Dot_position
{
  int pos_;
  Direction dir_;
  Grob *dot_;
  Box dot_extents_;
  Interval x_extent_;

  Dot_position ()
  {
    dot_ = 0;
    pos_ = 0;
    dir_ = CENTER;
  }
};

class Dot_configuration : private std::map<int, Dot_position>
{
public:
  Dot_formatting_problem const *problem_;

public:
  Dot_configuration (Dot_formatting_problem const &);
  Real x_offset () const;
  int badness () const;
  void print () const;
  Dot_configuration shifted (int k, Direction d) const;
  void remove_collision (int p);

public: // exposed subset of map interface
  using map::begin;
  using map::end;
  using map::operator[];
};

#endif
