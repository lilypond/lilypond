/*
  misc.cc -- implement various stuff

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999, 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>

#include "misc.hh"

#ifndef STANDALONE
#include "item.hh"
#endif

int
intlog2(int d) {
  int i=0;
  while (!(d&1)) 
    {
	d/= 2;
	i++;
    }
  assert (!(d/2));
  return i;
}

double
log_2(double x) {
  return log (x)  /log (2.0);
}

#ifndef STANDALONE
Interval
itemlist_width (const Array<Item*> &its)
{
  Interval iv ;
  iv.set_empty();
   
  for (int j =0; j < its.size(); j++)
    {
	iv.unite (its[j]->extent (X_AXIS));

    }
  return iv;
}

#endif


/*
  TODO
    group in some Array_*
    make more generic / templatise
 */
int
get_lower_bound (Array<Real> const& positions, Real x)
{
  if (x < positions[0])
    return 0;
  for (int i = 1; i < positions.size (); i++)
    if (x < positions[i])
      return i - 1;
  return positions.size () - 1;
}

Slice
get_bounds_slice (Array<Real> const& positions, Real x)
{
  int l = get_lower_bound (positions, x);
  int u = positions.size () - 1 <? l + 1;
  if (x < positions[l])
    u = l;
  return Slice (l, u);
}

Interval
get_bounds_iv (Array<Real> const& positions, Real x)
{
  Slice slice = get_bounds_slice (positions, x);
  return Interval (positions[slice.min ()], positions[slice.max ()]);
}

// silly name
Interval
quantise_iv (Array<Real> const& positions, Real period, Real x)
{
  /*
    ugh
    assume that 
      * positions are sorted, 
      * positions are nonnegative
      * period starts at zero
   */

  int n = (int)(x / period);
  Real frac = (x / period - n) * period;
  if (frac < 0)
    {
      frac += period;
      n--;
    }

  Slice slice = get_bounds_slice (positions, frac);
  Interval iv(positions[slice.min ()], positions[slice.max ()]);

  if (slice.min () == slice.max ())
    {
      if (slice.min () == 0)
	iv.min () = - period + positions.top ();
      else
	iv.max () = period + positions[0];
    }

  iv += period * n;

  return iv;
}
