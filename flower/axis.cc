/*
  axis.cc -- implement Axis

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "axis.hh"

#include "string.hh"

String
axis_name_string (Axis a)
{
  return to_string (char (a + 'x'));
}
