/*
  ineq-constrained-qp.cc -- implement Ineq_constrained_qp

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "ineq-constrained-qp.hh"
#include "qlpsolve.hh"
#include "debug.hh"
#include "choleski.hh"
#include "main.hh"

/*
  May be this also should go into a library
 */

const int MAXITER=100;		// qlpsolve.hh


/*
  assume x (idx) == value, and adjust constraints, lin and quad accordingly

  TODO: add const_term
  */
void
Ineq_constrained_qp::eliminate_var (int idx, Real value)
{
  Vector row (quad_.row (idx));
  row*= value;

  quad_.delete_row (idx);

  quad_.delete_column (idx);

  lin_.del (idx);
  row.del (idx);
  lin_ +=row ;

  for (int i=0; i < cons_.size(); i++)
    {
      consrhs_[i] -= cons_[i](idx) *value;
      cons_[i].del (idx);
    }
}

void
Ineq_constrained_qp::add_inequality_cons (Vector c, double r)
{
  cons_.push (c);
  consrhs_.push (r);
}

Ineq_constrained_qp::Ineq_constrained_qp (int novars):
  quad_ (novars),
  lin_ (novars),
  const_term_ (0.0)
{
}

void
Ineq_constrained_qp::OK() const
{
#if !defined (NDEBUG) && defined (PARANOID)
  assert (cons_.size() == consrhs_.size ());
  Matrix Qdif= quad_ - quad_.transposed();
  assert (Qdif.norm()/quad_.norm () < EPS);
#endif
}


Real
Ineq_constrained_qp::eval (Vector v)
{
  return v * quad_ * v + lin_ * v + const_term_;
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

  Active_constraints act (this);
  act.OK();


  Vector x (start);
  Vector gradient=quad_*x+lin_;
  //    Real fvalue = x*quad_*x/2 + lin*x + const_term;
  // it's no use.

  Vector last_gradient (gradient);
  int iterations=0;

  while (iterations++ < MAXITER)
    {
      //#ifdef PARANOID
      if (experimental_features_global_b)
	assert_solution (x);
      //#endif
      
      Vector direction= - act.find_active_optimum (gradient);

      DOUT << "gradient "<< gradient<< "\ndirection " << direction<<"\n";

      if (direction.norm() > EPS)
	{
	  DOUT << act.status() << '\n';

	  Real unbounded_alfa = 1.0;
	  Real minalf = unbounded_alfa;

	  Inactive_iter minidx (act);


	  /*
	    we know the optimum on this "hyperplane". Check if we
	    bump into the edges of the simplex
	    */

	  for (Inactive_iter ia (act); ia.ok(); ia++)
	    {
	      Real dot = ia.vec() * direction;
	      if (dot >= 0)
		continue;

	      
	      Real numerator = ia.rhs () - ia.vec()*x;
	      if (numerator >= 0)
		{
		  if (numerator > EPS)
		    warning (String ("Ineq_constrained_qp::solve (): Constraint off by ") + numerator);
		  minalf = -numerator;
		  minidx = ia;
		  break;
		}

	      Real alfa = numerator / dot;


	      if (minalf > alfa)
		{
		  minidx = ia;
		  minalf = alfa;
		}
	    }

	  Real optimal_step = minalf;

	  Vector deltax=direction * optimal_step;
	  x += deltax;
	  gradient += optimal_step * (quad_ * deltax);

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
    WARN<<_("didn't converge!\n");

  DOUT <<  ": found " << x<<" in " << iterations <<" iterations\n";
  assert_solution (x);
  return x;
}


Vector
Ineq_constrained_qp::solve (Vector start) const
{
  /* no hassle if no constraints*/
  if (! cons_.size())
    {
      Choleski_decomposition chol (quad_);
      return - chol.solve (lin_);
    }
  else
    {
      return constraint_solve (start);
    }
}

int
Ineq_constrained_qp::dim () const
{
  return lin_.dim();
}

