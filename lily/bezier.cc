/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "bezier.hh"

#include "warn.hh"

using std::vector;

Real binomial_coefficient_3[] = {1, 3, 3, 1};

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
rotate (vector<Offset> *array, Real deg)
{
  Offset rot (offset_directed (deg));
  for (vsize i = 0; i < array->size (); i++)
    (*array)[i] = complex_multiply (rot, (*array)[i]);
}

void
translate (vector<Offset> *array, Offset o)
{
  for (vsize i = 0; i < array->size (); i++)
    (*array)[i] += o;
}

Bezier::Bezier (const ly_scm_list &control_points)
{
  // Offsets in control_ are initialized to zero by default.  We could to save
  // a little time by adding a way to skip that (explicitly).
  size_t i = 0;
  for (SCM point : control_points)
    {
      if (i >= CONTROL_COUNT)
        break;
      control_[i++] = from_scm<Offset> (point);
    }
}

/*
  Formula of the bezier 3-spline

  sum_{j = 0}^3 (3 over j) z_j (1-t)^ (3-j)  t^j


  A is the axis of X coordinate.
*/

Real
Bezier::get_other_coordinate (Axis a, Real x) const
{
  auto other = other_axis (a);
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

vector<Real>
Bezier::get_other_coordinates (Axis a, Real x) const
{
  Axis other = other_axis (a);
  vector<Real> ts = solve_point (a, x);
  vector<Real> sols;
  for (vsize i = 0; i < ts.size (); i++)
    sols.push_back (curve_coordinate (ts[i], other));
  return sols;
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
      r += control_[j][a] * binomial_coefficient_3[j] * tj * one_min_tj[3 - j];

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
      o += control_[j] * binomial_coefficient_3[j] * tj * one_min_tj[3 - j];

      tj *= t;
    }

#ifdef PARANOID
  assert (fabs (o[X_AXIS] - polynomial (X_AXIS).eval (t)) < 1e-8);
  assert (fabs (o[Y_AXIS] - polynomial (Y_AXIS).eval (t)) < 1e-8);
#endif

  return o;
}

// The return value is normalized unless zero or indefinite.
Offset
Bezier::dir_at_point (Real t) const
{
  Offset second_order[3];
  for (vsize i = 0; i < 3; i++)
    second_order[i] = ((control_[i + 1] - control_[i]) * t) + control_[i];

  Offset third_order[2];
  for (vsize i = 0; i < 2; i++)
    third_order[i]
      = ((second_order[i + 1] - second_order[i]) * t) + second_order[i];

  return (third_order[1] - third_order[0]).direction ();
}

/*
  Cache binom (3, j) t^j (1-t)^{3-j}
*/
struct Polynomial_cache
{
  Polynomial terms_[4];
  Polynomial_cache ()
  {
    for (int j = 0; j <= 3; j++)
      terms_[j] = binomial_coefficient_3[j]
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

  Polynomial combine = xp * deriv[Y_AXIS] - yp * deriv[X_AXIS];

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
   For the portion of the curve between L and R along axis AX,
   return the bounding box limit in direction D along the cross axis to AX.
   If there is no portion between L and R, return 0.0 and report error.
*/
Real
Bezier::minmax (Axis ax, Real l, Real r, Direction d) const
{
  Axis bx = other_axis (ax);

  // The curve could hit its bounding box limit along BX at:
  //  points where the curve is parallel to AX,
  Offset vec (0.0, 0.0);
  vec[ax] = 1.0;
  vector<Real> sols (solve_derivative (vec));
  //  or endpoints of the curve,
  sols.push_back (0.999);
  sols.push_back (0.001);
  // (using points just inside the ends, so that an endpoint is evaulated
  //  if it falls within rounding error of L or R and the curve lies inside)

  Interval iv;
  for (vsize i = sols.size (); i--;)
    {
      Offset p (curve_point (sols[i]));
      if (p[ax] >= l && p[ax] <= r)
        iv.add_point (p[bx]);
    }

  //  or intersections of the curve with the bounding lines at L and R.
  Interval lr (l, r);
  for (const auto dir : {LEFT, RIGHT})
    {
      vector<Real> v = get_other_coordinates (ax, lr[dir]);
      for (vsize i = v.size (); i--;)
        iv.add_point (v[i]);
    }

  if (iv.is_empty ())
    {
      programming_error ("Bezier curve does not cross region of concern");
      return 0.0;
    }

  return iv.at (d);
}

/**
   Compute the bounding box dimensions in direction of A.
*/
Interval
Bezier::extent (Axis a) const
{
  Offset d;
  d[other_axis (a)] = 1.0;
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
Bezier::rotate (Real deg)
{
  Offset rot (offset_directed (deg));
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
    assert (!std::isnan (control_[i].length ())
            && !std::isinf (control_[i].length ()));
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
Bezier::subdivide (Real t, Bezier *left_part, Bezier *right_part) const
{
  Offset p[CONTROL_COUNT][CONTROL_COUNT];

  for (int i = 0; i < CONTROL_COUNT; i++)
    p[i][CONTROL_COUNT - 1] = control_[i];
  for (int j = CONTROL_COUNT - 2; j >= 0; j--)
    for (int i = 0; i < CONTROL_COUNT - 1; i++)
      p[i][j] = p[i][j + 1] + t * (p[i + 1][j + 1] - p[i][j + 1]);
  for (int i = 0; i < CONTROL_COUNT; i++)
    {
      left_part->control_[i] = p[0][CONTROL_COUNT - 1 - i];
      right_part->control_[i] = p[i][i];
    }
}

/*
  Extract a portion of a bezier from T_MIN to T_MAX
*/

Bezier
Bezier::extract (Real t_min, Real t_max) const
{
  if ((t_min < 0) || (t_max) > 1)
    programming_error (
      "bezier extract arguments outside of limits: curve may have bad shape");
  if (t_min >= t_max)
    programming_error ("lower bezier extract value not less than upper value: "
                       "curve may have bad shape");
  Bezier bez1, bez2, bez3, bez4;
  if (t_min == 0.0)
    bez2 = *this;
  else
    subdivide (t_min, &bez1, &bez2);
  if (t_max == 1.0)
    return bez2;
  else
    {
      bez2.subdivide ((t_max - t_min) / (1 - t_min), &bez3, &bez4);
      return bez3;
    }
}
