/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include "bezier.hh"
#include "polynomial.hh"

/*

  Formula of the bezier 3-spline

  sum_{j=0}^3  (3 over j) z_j (1-t)^(3-j)  t^j
 */

Bezier::Bezier ()
{
}

Real
Bezier::get_other_coordinate (Axis a,  Real x) const
{
  Axis other = Axis ((a +1)%NO_AXES);
  Array<Real> ts = solve_point (a, x);
  
  Offset c = curve_point (ts[0]);
  assert (fabs (c[a] - x) < 1e-8);
  
  return c[other];
}

Real
binomial_coefficient (Real over , int under)
{
  Real x = 1.0;

  while (under)
    {
      x *= over / Real (under);

      over  -= 1.0;
      under --;
    }
  return x;
}

Offset
Bezier::curve_point (Real t)const
{
  Real tj = 1;
  Real one_min_tj =  (1-t)*(1-t)*(1-t);

  Offset o;
  for (int j=0 ; j < 4; j++)
    {
      o += control_[j] * binomial_coefficient (3, j)
	* pow (t,j) * pow (1-t, 3-j);

      tj *= t;
      if (1-t)
	one_min_tj /= (1-t);
    }

  assert (fabs (o[X_AXIS] - polynomial (X_AXIS).eval (t))< 1e-8);
  assert (fabs (o[Y_AXIS] - polynomial (Y_AXIS).eval (t))< 1e-8);

  
  return o;
}


Polynomial
Bezier::polynomial (Axis a)const
{
  Polynomial p (0.0);
  for (int j=0; j <= 3; j++)
    {
      p += control_[j][a]
	* Polynomial::power (j , Polynomial (0,1))*
	Polynomial::power (3 - j, Polynomial (1,-1))*
	binomial_coefficient(3, j);
    }

  return p;
}

/**
   Remove all numbers outside [0,1] from SOL
 */
Array<Real>
filter_solutions (Array<Real> sol)
{
  for (int i = sol.size (); i--;)
    if (sol[i] < 0 || sol[i] >1)
      sol.del (i);
  return sol;
}

/**
   find t such that derivative is proportional to DERIV
 */
Array<Real>
Bezier::solve_derivative (Offset deriv)const
{
  Polynomial xp=polynomial (X_AXIS);
  Polynomial yp=polynomial (Y_AXIS);
  xp.differentiate ();
  yp.differentiate ();
  
  Polynomial combine = xp * deriv[Y_AXIS] - yp * deriv [X_AXIS];

  return filter_solutions (combine.solve ());
}
  

/*
  Find t such that curve_point (t)[AX] == COORDINATE
*/
Array<Real> 
Bezier::solve_point (Axis ax, Real coordinate) const
{
  Polynomial p(polynomial (ax));
  p.coefs_[0] -= coordinate;
  
  Array<Real> sol (p.solve ());
  return filter_solutions (sol);
}

Interval
Bezier::extent (Axis a)const
{
  int o = (a+1)%NO_AXES;
  Offset d;
  d[Axis (o)] =1.0;
  Interval iv;
  Array<Real> sols (solve_derivative (d));
  sols.push (1.0);
  sols.push (0.0);  
  for (int i= sols.size (); i--;)
    {
      Offset o (curve_point (sols[i]));
      iv.unite (Interval (o[a],o[a]));
    }
  return iv;
}

void
Bezier::flip (Axis a)
{
  for (int i = CONTROL_COUNT; i--;)
    control_[i][a] = - control_[i][a];
}

void
Bezier::rotate (Real phi)
{
  Offset rot (complex_exp (Offset (0, phi)));
  for (int i = 0; i < CONTROL_COUNT; i++)
    control_[i] = complex_multiply (rot, control_[i]);
}

void
Bezier::translate (Offset o)
{
  for (int i = 0; i < CONTROL_COUNT; i++)
    control_[i] += o;
}

void
Bezier::check_sanity () const
{
  for (int i=0; i < CONTROL_COUNT; i++)
    assert (!isnan (control_[i].length ())
	    && !isinf (control_[i].length ()));
}

void
Bezier::reverse ()
{
  Bezier b2;
  for (int i =0; i < CONTROL_COUNT; i++)
    b2.control_[CONTROL_COUNT-i-1] = control_[i];
  *this = b2;
}
