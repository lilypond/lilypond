/*
  qlp.hh -- declare Ineq_constrained_qp, Mixed_qp

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef QLP_HH
#define QLP_HH

#include "matrix.hh"

/// inequality constrained quadratic program
class Ineq_constrained_qp {
    friend class Active_constraints;

    Array<Vector> cons;
    Array<Real> consrhs;
public:
    Matrix quad;
    Vector lin;
    Real const_term;


    /**
      use a KKT method to assert optimality of sol
      */
    void assert_solution(Vector sol) const;
    /// solve the problem using a projected gradient method
    Vector solve(Vector start) const;
    
    /** return the number of variables in the problem */
    int dim() const{
	return lin.dim();
    }

    /**
      add a constraint


        c*vars >= r

      PRE
      c.dim() == dim();
	
      */
    void add_inequality_cons(Vector c, double r);
    
    /** set up matrices to go with the problem. */
    Ineq_constrained_qp(int novars);
    
    /**
    evaluate the quadratic function for input #v#
    */
    Real eval(Vector v);

    void eliminate_var(int idx, Real value);
    void OK()const;
    void print() const;

};

/// Quadratic programming with mixed linear constraints
/**
  problem definition of a quadratic optimisation problem with linear
  inequality and equality constraints


    x^T QUAD x /2 + b^T x 
*/
class Mixed_qp :public Ineq_constrained_qp {
    Array<int> eq_cons;
    Array<Real> eq_consrhs;
public:
    Mixed_qp(int n);
    void OK() const;
    void print() const;

    Vector solve(Vector start) const;
    void add_fixed_var(int i , Real value);
    

    /**
      add a constraint,

        c*vars == r

      PRE
      c.dim()==dim();
     */
    void add_equality_cons(Vector c, double r);
};
#endif
