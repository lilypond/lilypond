/*
  interval-set.hh -- declare Interval_set

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef INTERVAL_SET_HH
#define INTERVAL_SET_HH

#include "std-vector.hh"
#include "interval.hh"

/*
  A union of intervals in the real line.

  Abysmal performance (quadratic) for large N, hopefully we don't have
  that large N. In any case, this should probably be rewritten to use
  a balanced tree.
*/
struct Interval_set
{
  vector<Interval> allowed_regions_;

  Interval_set ();
  void set_full ();
  void remove_interval (Interval rm);
};

#endif /* INTERVAL_SET_HH */
