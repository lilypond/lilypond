/*
  interval-set.hh -- implement Interval_set

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
	      allowed_regions_.insert (before, i);
	      i++;
	    }
	  allowed_regions_.del (i);
	  if (!after.is_empty () && after.length () > 0.0)
	    {
	      allowed_regions_.insert (after, i);
	      i++;
	    }
	}
      else
	i++;
    }
}
