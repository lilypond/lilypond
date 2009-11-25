/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "interval-set.hh"

/*
  A union of intervals in the real line.

  Abysmal performance (quadratic) for large N, hopefully we don't have
  that large N. In any case, this should probably be rewritten to use
  a balanced tree.
*/

Interval_set::Interval_set ()
{
  set_full ();
}

void
Interval_set::set_full ()
{
  allowed_regions_.clear ();
  Interval s;
  s.set_full ();
  allowed_regions_.push_back (s);
}

void
Interval_set::remove_interval (Interval rm)
{
  for (vsize i = 0; i < allowed_regions_.size ();)
    {
      Interval s = rm;

      s.intersect (allowed_regions_[i]);

      if (!s.is_empty ())
	{
	  Interval before = allowed_regions_[i];
	  Interval after = allowed_regions_[i];

	  before[RIGHT] = s[LEFT];
	  after[LEFT] = s[RIGHT];

	  if (!before.is_empty () && before.length () > 0.0)
	    {
	      allowed_regions_.insert (allowed_regions_.begin () + i, before);
	      i++;
	    }
	  allowed_regions_.erase (allowed_regions_.begin () + i);
	  if (!after.is_empty () && after.length () > 0.0)
	    {
	      allowed_regions_.insert (allowed_regions_.begin () + i, after);
	      i++;
	    }
	}
      else
	i++;
    }
}
