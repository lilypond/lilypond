/*
  colhpos.cc -- implement Column_x_positions

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "column-x-positions.hh"
#include "real.hh"
#include "debug.hh"

Column_x_positions::Column_x_positions()
{
  energy_f_ = infinity_f;
  satisfies_constraints_b_ = false;
  force_f_ = 0;
}









