/*
  colhpos.hh -- part of GNU LilyPond

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef COLHPOS_HH
#define COLHPOS_HH

#include "varray.hh"
#include "lily-proto.hh"

typedef Array<Paper_column*>  Line_of_cols;

struct Col_hpositions {
  Line_spacer * spacer_l_;
  bool ugh_b_;
  Line_of_cols error_col_l_arr_;
  Line_of_cols cols;
  Array<Real> config;
  Real energy_f_;
  bool satisfies_constraints_b_;

  /* ************** */
  void OK() const;
  ~Col_hpositions();
  void solve_line();
  void approximate_solve_line();
  /** generate a solution with no regard to idealspacings or
    constraints.  should always work */
  void stupid_solution();
  void set_stupid_solution (Vector);
  Col_hpositions();
  void add (Paper_column*c);
  void print() const;
};


#endif // COLHPOS_HH

