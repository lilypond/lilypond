/*
  column-x-positions.hh -- part of GNU LilyPond

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef COLUMN_X_POSITIONS_HH
#define COLUMN_X_POSITIONS_HH

#include "parray.hh"
#include "lily-proto.hh"

typedef Link_array<Paper_column>  Line_of_cols;

struct Column_x_positions {

  Line_of_cols cols_;
  Array<Real> config_;
  
  Real force_f_;
  Real energy_f_;
  bool satisfies_constraints_b_;

  ~Column_x_positions();
  Column_x_positions();
  void print() const;
};


#endif // COLUMN_X_POSITIONS_HH

