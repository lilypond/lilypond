/*
  axis.cc -- implement Axis

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>

#include "axes.hh"
#include "string.hh"

String
axis_name_str (Axis a)
{
  return String (char(a + 'x'));
}

/*
  TODO inline these.
 */
Axis
post_incr(Axis &a)
{
  assert(a < NO_AXES);
  Axis b= a;
  a = Axis(int(a) + 1);
  return b;
}

Axis
incr(Axis &a)
{
  assert(a < NO_AXES);
  a = Axis(int(a) + 1);
  return a;
}

