#ifndef QLP_HH
#define QLP_HH

#include "matrix.hh"

/// inequality constrained quadratic program
class Ineq_constrained_qp {
    friend class Active_constraints;

    svec<Vector> cons;
    svec<Real> consrhs;
public:
    Matrix quad;
    Vector lin;


    ///
    void assert_solution(Vector sol) const;
    /**
      use a KKT method to assert optimality of sol
      */
    /// solve the problem using a variable metric method
    Vector solve(Vector start) const;
    
    int dim() const{
	return lin.dim();
    }
    /** return the number of variables in the problem */
    ///
    void add_inequality_cons(Vector c, double r);
    /**
      add a constraint


        c*vars >= r

      PRE
      c.dim() == dim();
	
      */
    ///
    Ineq_constrained_qp(int novars);
    /** set up matrices to go with the problem. */

    Real eval(Vector v);
    /**
    evaluate the quadratic function for input #v#
    */

    void eliminate_var(int idx, Real value);
    void OK()const;
    void print() const;

};

/// Quadratic programming with mixed linear constraints
class Mixed_qp :public Ineq_constrained_qp {
    svec<int> eq_cons;
    svec<Real> eq_consrhs;
public:
    Mixed_qp(int n);
    void OK() const;
    void print() const;

    Vector solve(Vector start) const;
    void add_fixed_var(int i , Real value);
    
    ///
    void add_equality_cons(Vector c, double r);
    /**
      add a constraint,

        c*vars == r

      PRE
      c.dim()==dim();
     */

};
/**
  problem definition of a quadratic optimisation problem with linear
  inequality and equality constraints


    x^T QUAD x /2 + b^T x 
*/

typedef Mixed_qp Optimisation_problem;
#endif
