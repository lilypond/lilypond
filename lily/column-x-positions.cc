/*
  colhpos.cc -- implement Column_x_positions

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "column-x-positions.hh"

Column_x_positions::Column_x_positions ()
{
  satisfies_constraints_ = true;
  force_ = 0;
}
