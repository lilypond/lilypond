/*   
  linear-programming.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.ruu.nl>
  
 */

#ifndef LINEAR_PROGRAMMING_HH
#define LINEAR_PROGRAMMING_HH

#include "linear-programming.hh"

/**

   min c* x

   constraints_[i] * x = constraint_rhss_ [i]
   
   x[j] >= 0
*/

class Linear_programming
{
  Array<Vector> constraints_;
  Array<Real> constraint_rhss_;
  Vector cost_vec_;

public:
  Vector constraint_solve (Vector initial) const;
  int dim () const;
  Vector solve (Vector) const;
  void add_constraint (Vector c, double r);
  Linear_programming (int n);
  void set_cost (Vector);
  void print () const;
  void OK () const;
}

#endif /* LINEAR_PROGRAMMING_HH */

