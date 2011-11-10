/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Mike Solomon <mike@apollinemike.com>

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

#ifndef COLUMN_DESCRIPTION_HH
#define COLUMN_DESCRIPTION_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "spring.hh"

struct Rod_description
{
  vsize r_;
  Real dist_;

  bool operator < (const Rod_description r)
  {
    return r_ < r.r_;
  }

  Rod_description ()
  {
    r_ = 0;
    dist_ = 0;
  }

  Rod_description (vsize r, Real d)
  {
    r_ = r;
    dist_ = d;
  }
};

struct Column_description
{
  vector<Rod_description> rods_;
  vector<Rod_description> end_rods_;   /* use these if they end at the last column of the line */
  Spring spring_;
  Spring end_spring_;

  SCM break_permission_;
  Interval keep_inside_line_;

  Column_description ()
  {
    break_permission_ = SCM_EOL;
  }
  static Column_description get_column_description (vector<Grob *> const &cols, vsize col_index, bool line_starter);
};

#endif /* COLUMN_DESCRIPTION_HH */
