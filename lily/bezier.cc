/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "bezier.hh"
#include "warn.hh"
#include "libc-extension.hh"

Real binomial_coefficient_3[] = {
  1, 3, 3, 1
};

void
scale (vector<Offset> *array, Real x, Real y)
{
  for (vsize i = 0; i < array->size (); i++)
    {
      (*array)[i][X_AXIS] = x * (*array)[i][X_AXIS];
      (*array)[i][Y_AXIS] = y * (*array)[i][Y_AXIS];
    }
}

void
rotate (vector<Offset> *array, Real phi)
{
  Offset rot (complex_exp (Offset (0, phi)));
  for (vsize i = 0; i < array->size (); i++)
    (*array)[i] = complex_multiply (rot, (*array)[i]);
}

void
translate (vector<Offset> *array, Offset o)
{
  for (vsize i = 0; i < array->size (); i++)
    (*array)[i] += o;
}

/*
  Formula of the bezier 3-spline

  sum_{j = 0}^3 (3 over j) z_j (1-t)^ (3-j)  t^j


  A is the axis of X coordinate.
*/

Real
Bezier::get_other_coordinate (Axis a, Real x) const
{
  Axis other = Axis ((a +1) % NO_AXES);
  vector<Real> ts = solve_point (a, x);

  if (ts.size () == 0)
    {
      programming_error ("no solution found for Bezier intersection");
      return 0.0;
    }

#ifdef PARANOID
  Offset c = curve_point (ts[0]);
  if (fabs (c[a] - x) > 1e-8)
    programming_error ("bezier intersection not correct?");
#endif

  return curve_coordinate (ts[0], other);
}

Real
Bezier::curve_coordinate (Real t, Axis a) const
{
  Real tj = 1;
  Real one_min_tj[4];
  one_min_tj[0] = 1;
  for (int i = 1; i < 4; i++)
    one_min_tj[i] = one_min_tj[i - 1] * (1 - t);

  Real r = 0.0;
  for (int j = 0; j < 4; j++)
    {
      r += control_[j][a] * binomial_coefficient_3[j]
	* tj * one_min_tj[3 - j];

      tj *= t;
    }

  return r;
}

Offset
Bezier::curve_point (Real t) const
{
  Real tj = 1;
  Real one_min_tj[4];
  one_min_tj[0] = 1;
  for (int i = 1; i < 4; i++)
    one_min_tj[i] = one_min_tj[i - 1] * (1 - t);

  Offset o;
  for (int j = 0; j < 4; j++)
    {
      o += control_[j] * binomial_coefficient_3[j]
	* tj * one_min_tj[3 - j];

      tj *= t;
    }

#ifdef PARANOID
  assert (fabs (o[X_AXIS] - polynomial (X_AXIS).eval (t)) < 1e-8);
  assert (fabs (o[Y_AXIS] - polynomial (Y_AXIS).eval (t)) < 1e-8);
#endif

  return o;
}

/*
  Cache binom (3, j) t^j (1-t)^{3-j}
*/
struct Polynomial_cache {
  Polynomial terms_[4];
  Polynomial_cache ()
  {
    for (int j = 0; j <= 3; j++)
      terms_[j]
	= binomial_coefficient_3[j]
	* Polynomial::power (j, Polynomial (0, 1))
	* Polynomial::power (3 - j, Polynomial (1, -1));
  }
};

static Polynomial_cache poly_cache;

Polynomial
Bezier::polynomial (Axis a) const
{
  Polynomial p (0.0);
  Polynomial q;
  for (int j = 0; j <= 3; j++)
    {
      q = poly_cache.terms_[j];
      q *= control_[j][a];
      p += q;
    }

  return p;
}

/**
   Remove all numbers outside [0, 1] from SOL
*/
vector<Real>
filter_solutions (vector<Real> sol)
{
  for (vsize i = sol.size (); i--;)
    if (sol[i] < 0 || sol[i] > 1)
      sol.erase (sol.begin () + i);
  return sol;
}

/**
   find t such that derivative is proportional to DERIV
*/
vector<Real>
Bezier::solve_derivative (Offset deriv) const
{
  Polynomial xp = polynomial (X_AXIS);
  Polynomial yp = polynomial (Y_AXIS);
  xp.differentiate ();
  yp.differentiate ();

  Polynomial combine = xp * deriv[Y_AXIS] - yp * deriv [X_AXIS];

  return filter_solutions (combine.solve ());
}

/*
  Find t such that curve_point (t)[AX] == COORDINATE
*/
vector<Real>
Bezier::solve_point (Axis ax, Real coordinate) const
{
  Polynomial p (polynomial (ax));
  p.coefs_[0] -= coordinate;

  vector<Real> sol (p.solve ());
  return filter_solutions (sol);
}

/**
   Compute the bounding box dimensions in direction of A.
*/
Interval
Bezier::extent (Axis a) const
{
  int o = (a + 1)%NO_AXES;
  Offset d;
  d[Axis (o)] = 1.0;
  Interval iv;
  vector<Real> sols (solve_derivative (d));
  sols.push_back (1.0);
  sols.push_back (0.0);
  for (vsize i = sols.size (); i--;)
    {
      Offset o (curve_point (sols[i]));
      iv.unite (Interval (o[a], o[a]));
    }
  return iv;
}

Interval
Bezier::control_point_extent (Axis a) const
{
  Interval ext;
  for (int i = CONTROL_COUNT; i--;)
    ext.add_point (control_[i][a]);

  return ext;      
}


/**
   Flip around axis A
*/
void
Bezier::scale (Real x, Real y)
{
  for (int i = CONTROL_COUNT; i--;)
    {
      control_[i][X_AXIS] = x * control_[i][X_AXIS];
      control_[i][Y_AXIS] = y * control_[i][Y_AXIS];
    }
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
Bezier::assert_sanity () const
{
  for (int i = 0; i < CONTROL_COUNT; i++)
    assert (!isnan (control_[i].length ())
	    && !isinf (control_[i].length ()));
}

void
Bezier::reverse ()
{
  Bezier b2;
  for (int i = 0; i < CONTROL_COUNT; i++)
    b2.control_[CONTROL_COUNT - i - 1] = control_[i];
  *this = b2;
}


/*
  Subdivide a bezier at T into LEFT_PART and RIGHT_PART
  using deCasteljau's algorithm.
*/
void
Bezier::subdivide (Real t, Bezier &left_part, Bezier &right_part)
{
  Offset p[CONTROL_COUNT][CONTROL_COUNT];

  for (int i = 0; i < CONTROL_COUNT ; i++)
    p[i][CONTROL_COUNT - 1 ] = control_[i];
  for (int j = CONTROL_COUNT - 2; j >= 0 ; j--)
  for (int i = 0; i < CONTROL_COUNT -1; i++)
    p[i][j] = p[i][j+1] + t * (p[i+1][j+1] - p[i][j+1]);
  for (int i = 0; i < CONTROL_COUNT; i++)
    {
      left_part.control_[i]=p[0][CONTROL_COUNT - 1 - i];
      right_part.control_[i]=p[i][i];
    }
}

/*
  Extract a portion of a bezier from T_MIN to T_MAX
*/

Bezier
Bezier::extract (Real t_min, Real t_max)
{
  Bezier bez1, bez2, bez3, bez4;
  if (t_min == 0.0)
    {
      for (int i = 0; i < CONTROL_COUNT; i++)
        bez2.control_[i] = control_[i];
    }
  else
    {
      subdivide (t_min, bez1, bez2);
    }
  if (t_max == 1.0)
    {
      return bez2;
    }
  else
   {
     bez2.subdivide ((t_max-t_min)/(1-t_min), bez3, bez4);
     return bez3;
  }
}
