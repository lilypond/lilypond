/*
  axis.cc -- implement Axis



  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <assert.h>
#include "axes.hh"
#include "string.hh"

String
axis_name_string (Axis a)
{
  return to_string (char (a + 'x'));
}


Axis
other_axis (Axis a)
{
  return a ==  Y_AXIS ? X_AXIS : Y_AXIS;
}

/*
  TODO inline these.
 */
Axis
post_incr (Axis &a)
{
  assert (a < NO_AXES);
  Axis b= a;
  a = Axis (int (a) + 1);
  return b;
}

Axis
incr (Axis &a)
{
  assert (a < NO_AXES);
  a = Axis (int (a) + 1);
  return a;
}

