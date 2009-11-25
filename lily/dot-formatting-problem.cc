/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "dot-formatting-problem.hh"
#include "dot-configuration.hh"
#include "skyline.hh"

Dot_formatting_problem::~Dot_formatting_problem()
{
  delete best_;
}

void
Dot_formatting_problem::register_configuration (Dot_configuration const &src)
{
  int b = src.badness ();
  if (b < score_)
    {
      delete best_;
      best_ = new Dot_configuration (src);
    }
}

Dot_configuration *
Dot_formatting_problem::best () const
{
  return best_;
}

Dot_formatting_problem::Dot_formatting_problem (vector<Box> const &boxes,
						Interval base_x)
  : head_skyline_ (boxes, 0.0, Y_AXIS, RIGHT)
{
  best_ = 0;
  head_skyline_.set_minimum_height (base_x[RIGHT]);
  score_ = 1 << 30;
}
