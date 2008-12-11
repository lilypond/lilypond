/*
  column-x-positions.cc -- implement Column_x_positions

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "column-x-positions.hh"

Column_x_positions::Column_x_positions ()
{
  satisfies_constraints_ = true;
  force_ = 0;
}
