/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2012 Mike Solomon <mike@mikesolomon.org>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "box-quarantine.hh"
#include <functional>

Box_quarantine::Box_quarantine (Real padding, Axis a)
  : padding_ (padding), a_ (a)
{
}

void
Box_quarantine::add_box_to_quarantine (Box infected)
{
  Box nbox (infected);
  nbox[a_].widen (padding_ / 2);
  boxes_to_quarantine_.push_back (nbox);
}

vector<Box>
Box_quarantine::quarantined_boxes ()
{
  return boxes_to_quarantine_;
}

struct Intersection_info
{
  vsize index_;
  Real amount_;
  int mid_;
  Intersection_info (vsize index, Real amount, int mid);
};

Intersection_info::Intersection_info (vsize index, Real amount, int mid)
  : index_ (index), amount_ (amount), mid_ (mid)
{
}

/*
  boxes_to_quarantine_ contains a vector of boxes that may
  or may not overlap.  it iterates through these boxes,
  pushing quarantined_boxes_ epsilon over or epsilon under a
  collision.  when this type of change happens, the loop is marked
  as "dirty" and re-iterated.

  TODO: not sure if this loop causes infinite behavior...
*/

bool
sort_towards_middle(Intersection_info ii0, Intersection_info ii1)
{
  // ugh...with C++11, we'll just be able to do a factory instead of
  // bundling mid with the Intersection_info
  int mid = ii0.mid_;
  assert ((double)(ii0.index_ - mid) >= 0.0);
  assert ((double)(ii1.index_ - mid) >= 0.0);
  return fabs (ii0.index_ - mid) < fabs (ii1.index_ - mid);
}

void
Box_quarantine::solve ()
{
  int mid = boxes_to_quarantine_.size () / 2;
  Real epsilon = 1.0e-4;
  bool dirty = false;
  do
    {
      dirty = false;
      vector<Intersection_info> ii;
      for (vsize i = 0; i < boxes_to_quarantine_.size () - 1; i++)
        {
          Box scratch (boxes_to_quarantine_[i]);
          scratch.intersect (boxes_to_quarantine_[i + 1]);
          if (!scratch.is_empty ())
            ii.push_back (Intersection_info (i, scratch[a_].length (), mid));
        }
      vector_sort (ii, sort_towards_middle);
      if (ii.size () > 0)
        {
          Offset shift (-(epsilon + ii[0].amount_ / 2.0), 0.0);
          if (a_ == Y_AXIS)
            shift = shift.swapped ();
          boxes_to_quarantine_[ii[0].index_].translate (shift);
          shift[a_] = -shift[a_];
          boxes_to_quarantine_[ii[0].index_ + 1].translate (shift);
          dirty = true;
        }
    }
  while (dirty);
}
