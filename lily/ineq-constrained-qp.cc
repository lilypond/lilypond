/*
  ineq-constrained-qp.cc -- implement Ineq_constrained_qp

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "ineq-constrained-qp.hh"
#include "qlpsolve.hh"
#include "debug.hh"
#include "choleski.hh"

/*
  MAy be this also should go into a library
 */

const int MAXITER=100;		// qlpsolve.hh

/*
  assume x (idx) == value, and adjust constraints, lin and quad accordingly

  TODO: add const_term
  */
void
Ineq_constrained_qp::eliminate_var (int idx, Real value)
{
  Vector row (quad.row (idx));
  row*= value;

  quad.delete_row (idx);

  quad.delete_column (idx);

  lin.del (idx);
  row.del (idx);
  lin +=row ;

   for (int i=0; i < cons.size(); i++) 
   {
    consrhs[i] -= cons[i](idx) *value;
    cons[i].del (idx);
   }
}

void
Ineq_constrained_qp::add_inequality_cons (Vector c, double r)
{
  cons.push (c);
  consrhs.push (r);
}

Ineq_constrained_qp::Ineq_constrained_qp (int novars):
  quad (novars),
  lin (novars),
  const_term (0.0)
{
}

void
Ineq_constrained_qp::OK() const
{
#if !defined (NDEBUG) && defined (PARANOID)
  assert (cons.size() == consrhs.size ());
  Matrix Qdif= quad - quad.transposed();
  assert (Qdif.norm()/quad.norm () < EPS);
#endif    
}
   

Real
Ineq_constrained_qp::eval (Vector v)
{
  return v * quad * v + lin * v + const_term;
}


int
min_elt_index (Vector v)
{
  Real m=infinity_f; 
  int idx=-1;
  for (int i = 0; i < v.dim(); i++)
    {
	if (v (i) < m) 
	  {
	    idx = i;
	    m = v (i);
	  }
	assert (v (i) <= infinity_f);
    }
  return idx;
}


/**the numerical solving. Mordecai Avriel, Nonlinear Programming: analysis and methods (1976)
  Prentice Hall.

  Section 13.3

  This is a "projected gradient" algorithm. Starting from a point x
  the next point is found in a direction determined by projecting
  the gradient onto the active constraints.  (well, not really the
  gradient. The optimal solution obeying the active constraints is
  tried. This is why H = Q^-1 in initialisation))


  */
Vector
Ineq_constrained_qp::constraint_solve (Vector start) const 
{    
  if (!dim())
	return Vector (0);
  
  // experimental
  if (quad.dim() > 10)
	quad.try_set_band();
  
  Active_constraints act (this);
  act.OK();    

  
  Vector x (start);
  Vector gradient=quad*x+lin;
	//    Real fvalue = x*quad*x/2 + lin*x + const_term;
	// it's no use.

  Vector last_gradient (gradient);
  int iterations=0;
  
  while (iterations++ < MAXITER) 
    {
	Vector direction= - act.find_active_optimum (gradient);
     	
	DOUT << "gradient "<< gradient<< "\ndirection " << direction<<"\n";
	
	if (direction.norm() > EPS) 
	  {
	    DOUT << act.status() << '\n';
	    
	    Real minalf = infinity_f;

	    Inactive_iter minidx (act);


	    /*
	    we know the optimum on this "hyperplane". Check if we
	    bump into the edges of the simplex
	    */
  
	    for (Inactive_iter ia (act); ia.ok(); ia++) 
	      {

		if (ia.vec() * direction >= 0)
		    continue;
		Real alfa= - (ia.vec()*x - ia.rhs ())/
		    (ia.vec()*direction);
		
		if (minalf > alfa) 
		  {
		    minidx = ia;
		    minalf = alfa;
		  }
	      }
	    Real unbounded_alfa = 1.0;
	    Real optimal_step = min (minalf, unbounded_alfa);

	    Vector deltax=direction * optimal_step;
	    x += deltax;	    
	    gradient += optimal_step * (quad * deltax);
	    
	    DOUT << "step = " << optimal_step<< " (|dx| = " <<
		deltax.norm() << ")\n";	    
	   
	    if (minalf < unbounded_alfa) 
	      {
		/* bumped into an edge. try again, in smaller space. */
		act.add (minidx.idx());
		DOUT << "adding cons "<< minidx.idx()<<'\n';
		continue;
	      }
	    /*ASSERT: we are at optimal solution for this "plane"*/
  
  
	  }
	
	Vector lagrange_mult=act.get_lagrange (gradient);	
	int m= min_elt_index (lagrange_mult);
	
	if (m>=0 && lagrange_mult (m) > 0) 
	  {
	    break;		// optimal sol.
	  }
	else if (m<0) 
	  {
	    assert (gradient.norm() < EPS) ;
	    
	    break;
	  }
	
	DOUT << "dropping cons " << m<<'\n';
	act.drop (m);
    }
  if (iterations >= MAXITER)
	WARN<<"didn't converge!\n";
  
  DOUT <<  ": found " << x<<" in " << iterations <<" iterations\n";
  assert_solution (x);
  return x;
} 

  
Vector
Ineq_constrained_qp::solve (Vector start)const
{ 
  /* no hassle if no constraints*/
  if ( ! cons.size()) 
    {
	Choleski_decomposition chol (quad);
	return - chol.solve (lin);
    }
  else 
    {
	return constraint_solve (start);
    }
}
