/*
  qlp.hh -- declare Ineq_constrained_qp, Mixed_qp

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef QLP_HH
#define QLP_HH

#include "ineq-constrained-qp.hh"

/**
   Quadratic programming with mixed linear constraints.
  problem definition of a quadratic optimisation problem with linear
  inequality and equality constraints


    x^T QUAD x /2 + b^T x 
*/
class Mixed_qp :public Ineq_constrained_qp {
    Array<int> eq_cons;
    Array<Real> eq_consrhs;
public:
    Mixed_qp (int n);
    void OK() const;
    void print() const;

    Vector solve (Vector start) const;
    void add_fixed_var (int i , Real value);
    

    /**
      add a constraint,

        c*vars == r

      PRE
      c.dim()==dim ();
     */
    void add_equality_cons (Vector c, double r);
};
#endif
