/*
  gourlay-breaking.hh -- declare Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef GOURLAY_BREAKING_HH
#define GOURLAY_BREAKING_HH

#include "break.hh"

/**
  A dynamic programming solution to breaking scores into lines
 */
struct Gourlay_breaking : public Break_algorithm
{
    Real energy_bound_f_ ;
    
    /// maximum number of measures in a line
    int max_measures_i_;
    void do_set_pscore();
    Array<Col_hpositions> do_solve() const;
    Gourlay_breaking();
};
#endif // GOURLAY_BREAKING_HH
