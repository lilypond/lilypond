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

#ifndef DOT_FORMATTING_PROBLEM_HH
#define DOT_FORMATTING_PROBLEM_HH

#include "skyline.hh"

#include <vector>

class Dot_formatting_problem
{
private:
  Skyline head_skyline_;

public:
  Skyline const &head_skyline () const { return head_skyline_; }

  Dot_formatting_problem (std::vector<Box> const &boxes, Interval base_x);
};

#endif
