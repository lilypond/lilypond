/*
  ineq-constrained-qp.hh -- declare Ineq_constrained_qp

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef INEQ_CONSTRAINED_QP_HH
#define INEQ_CONSTRAINED_QP_HH



#include "matrix.hh"

/** inequality constrained quadratic program

    It takes the form of

    optimise for x : x*quad_ *x + lin_* x + const_term_

    subject to for all i:  cons_[i] * x >= consrhs_[i]


    @usage:
    instantiate Ineq_constrained_qp.
    
    Modify quad_, lin_ and const_term_ directly. Use
    add_inequality_cons () to add constraints.  Call solve () with a
    feasible solution

    
 */
class Ineq_constrained_qp {
    friend class Active_constraints;

    Array<Vector> cons_;
    Array<Real> consrhs_;
public:
    Matrix quad_;
    Vector lin_;
    Real const_term_;


    /**
      use a KKT method to assert optimality of sol
      */
    void assert_solution (Vector sol) const;
    /// solve the problem using a projected gradient method
    Vector constraint_solve (Vector) const;
    /**
      Solve it. First try it the easy way.
     */
    Vector solve (Vector start) const;
    
    /**
      @return the number of variables in the problem
      */
    int dim() const;

    /**
      add a constraint


        c*vars >= r

      PRE
      c.dim() == dim ();
	
      */
    void add_inequality_cons (Vector c, double r);
    
    /** set up matrices to go with the problem. */
    Ineq_constrained_qp (int novars);
    
    /**
    evaluate the quadratic function for input #v#
    */
    Real eval (Vector v);

    void eliminate_var (int idx, Real value);
    void OK() const;
    void print() const;

};

// ugh
const Real EPS=1e-7;		

#endif // INEQ_CONSTRAINED_QP_HH
