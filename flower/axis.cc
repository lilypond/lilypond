/*
  axis.cc -- implement Axis



  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axes.hh"

#include "string.hh"

String
axis_name_string (Axis a)
{
  return to_string (char (a + 'x'));
}
