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
      d /= 2;
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

