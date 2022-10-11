/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef COLUMN_X_POSITIONS_HH
#define COLUMN_X_POSITIONS_HH

#include "lily-proto.hh"
#include "real.hh"

#include <vector>

class Paper_column;

class Column_x_positions
{
public:
  std::vector<Paper_column *> cols_;
  std::vector<Paper_column *> loose_cols_;

  std::vector<Real> config_;
  Real force_;
  bool satisfies_constraints_;

  Column_x_positions ();
};

#endif // COLUMN_X_POSITIONS_HH
