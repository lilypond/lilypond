/*
  misc.cc -- implement various stuff

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include "misc.hh"

/*
  Return the 2-log, rounded down 
 */
int
intlog2 (int d)
{
  assert (d);
  int i=0;
  while ((d != 1)) 
    {
      d/= 2;
      i++;
    }
  
  assert (! (d/2));
  return i;
}

double
log_2 (double x)
{
  return log (x)  /log (2.0);
}


static int
comp (Real const &a, Real const &b)
{
  return sign (a-b);
}

Interval
quantise_iv (Array<Real> positions, Real x)
{
  positions.sort (comp);
  Real period = positions.top () - positions[0];
  
  int n =  int ((x - positions[0]) / period);
  Real frac = (x - positions[0]) -  n * period;

  while (frac < 0)
    {
      frac += period;
      n --;
    }
  
  Real px = frac + positions[0];
  assert (positions[0] <= px && px <= positions.top ());
  int i=0;
  for (; i < positions.size () - 1; i++)
    {
      if (positions[i] <= px && px <= positions[i+1])
	break; 
    }

  return Interval (positions[i] , positions[i+1]) + period * n;
}
