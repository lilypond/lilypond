/*
  colhpos.hh -- part of GNU LilyPond

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef COLHPOS_HH
#define COLHPOS_HH

#include "array.hh"
#include "lily-proto.hh"

typedef Array<Paper_column*>  Line_of_cols;

struct Column_x_positions {
  Line_spacer * spacer_l_;
  bool ugh_b_;
  Line_of_cols error_col_l_arr_;
  Line_of_cols cols;
  Array<Real> config;
  Real energy_f_;
  bool satisfies_constraints_b_;

  void OK() const;
  ~Column_x_positions();
  void solve_line();
  void approximate_solve_line();
  /** generate a solution with no regard to idealspacings or
    constraints.  should always work */
  void stupid_solution();
  void set_stupid_solution (Vector);
  Column_x_positions();
  void add_paper_column (Paper_column*c);
  void print() const;
};


#endif // COLHPOS_HH

