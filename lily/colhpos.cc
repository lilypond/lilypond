/*
  colhpos.cc -- implement Col_hpositions

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "colhpos.hh"
#include "real.hh"
#include "debug.hh"
#include "vector.hh"
#include "line-spacer.hh"

Col_hpositions::Col_hpositions()
{
  energy_f_ = infinity_f;
  ugh_b_ = false;
  satisfies_constraints_b_ = false;
  spacer_l_ =0;
}

Col_hpositions::~Col_hpositions()
{

}

void
Col_hpositions::add (Paper_column*c)
{
  cols.push (c);
}

void
Col_hpositions::print() const
{
#ifndef NPRINT
  DOUT << "energy : " << energy_f_ << '\n';
  DOUT << "line of " << config.size() << " cols\n";
  Vector v (config);
  DOUT << v;
#endif
}

void
Col_hpositions::OK() const
{
#ifndef NDEBUG
  assert (config.size() == cols.size ());
#endif
}

void
Col_hpositions::set_stupid_solution(Vector v)
{
  energy_f_ = infinity_f;
  ugh_b_ = true;
  config = v;
}

void
Col_hpositions::stupid_solution()
{
  set_stupid_solution (spacer_l_->default_solution());
}

void
Col_hpositions::solve_line() 
{
  spacer_l_->solve (this);
}


void
Col_hpositions::approximate_solve_line() 
{
  spacer_l_->lower_bound_solution (this);
}


