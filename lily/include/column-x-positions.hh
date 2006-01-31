/*
  column-x-positions.hh -- part of GNU LilyPond

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef COLUMN_X_POSITIONS_HH
#define COLUMN_X_POSITIONS_HH

#include "parray.hh"
#include "lily-proto.hh"

struct Column_x_positions
{
  Link_array<Grob> cols_;
  Link_array<Grob> loose_cols_;

  std::vector<Real> config_;
  Real force_;
  bool satisfies_constraints_;

  Column_x_positions ();
};

#endif // COLUMN_X_POSITIONS_HH

