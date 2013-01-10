/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Mike Solomon <mike@mikesolomon.org>

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

#ifndef BOX_QUARANTINE_HH
#define BOX_QUARANTINE_HH

#include "lily-proto.hh"
#include "std-vector.hh"
#include "box.hh"

class Box_quarantine
{
public:
  Box_quarantine (Real, Axis);
  void add_box_to_quarantine (Box);
  void solve ();
  vector<Box> quarantined_boxes ();

private:
  vector<Box> boxes_to_quarantine_;
  Real padding_;
  Axis a_;
};

#endif // BOX_QUARANTINE_HH
