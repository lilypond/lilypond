/*
  axis.cc -- implement Axis

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "axes.hh"
#include "string.hh"

String
axis_name_str(Axis a)
{
    return String (a + 'x');
}
