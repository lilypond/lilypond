/*
  axis.cc -- implement Axis



  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <assert.h>
#include "axes.hh"

String
axis_name_str (Axis a)
{
  return to_str (char(a + 'x'));
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

