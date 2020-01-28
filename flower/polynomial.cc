/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1993--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "polynomial.hh"

#include "warn.hh"

#include <cmath>

using std::vector;

/*
  Een beter milieu begint bij uzelf. Hergebruik!


  This was ripped from Rayce, a raytracer I once wrote.
*/

Real
Polynomial::eval (Real x) const
{
  Real p = 0.0;

  // horner's scheme
  for (vsize i = coefs_.size (); i--;)
    p = x * p + coefs_[i];

  return p;
}

Polynomial
Polynomial::multiply (const Polynomial &p1, const Polynomial &p2)
{
  Polynomial dest;

  ssize_t deg = p1.degree () + p2.degree ();
  for (ssize_t i = 0; i <= deg; i++)
    {
      dest.coefs_.push_back (0);
      for (ssize_t j = 0; j <= i; j++)
        if (i - j <= p2.degree () && j <= p1.degree ())
          dest.coefs_.back () += p1.coefs_[j] * p2.coefs_[i - j];
    }

  return dest;
}

Real
Polynomial::minmax (Real l, Real r, bool ret_max) const
{
  vector<Real> sols;
  if (l > r)
    {
      programming_error ("left bound greater than right bound for polynomial "
                         "minmax.  flipping bounds.");
      l = l + r;
      r = l - r;
      l = l - r;
    }

  sols.push_back (eval (l));
  sols.push_back (eval (r));

  Polynomial deriv (*this);
  deriv.differentiate ();
  vector<Real> maxmins = deriv.solve ();
  for (vsize i = 0; i < maxmins.size (); i++)
    if (maxmins[i] >= l && maxmins[i] <= r)
      sols.push_back (eval (maxmins[i]));
  vector_sort (sols, std::less<Real> ());

  return ret_max ? sols.back () : sols[0];
}

void
Polynomial::differentiate ()
{
  for (int i = 1; i <= degree (); i++)
    coefs_[i - 1] = coefs_[i] * i;
  coefs_.pop_back ();
}

Polynomial
Polynomial::power (int exponent, const Polynomial &src)
{
  int e = exponent;
  Polynomial dest (1), base (src);

  /*
    classic int power. invariant: src^exponent = dest * src ^ e
    greetings go out to Lex Bijlsma & Jaap vd Woude */
  while (e > 0)
    {
      if (e % 2)
        {
          dest = multiply (dest, base);
          e--;
        }
      else

        {
          base = multiply (base, base);
          e /= 2;
        }
    }
  return dest;
}

static Real const FUDGE = 1e-8;

void
Polynomial::clean ()
{
  /*
    We only do relative comparisons. Absolute comparisons break down in
    degenerate cases.  */
  while (degree () > 0
         && (fabs (coefs_.back ()) < FUDGE * fabs (back (coefs_, 1))
             || !coefs_.back ()))
    coefs_.pop_back ();
}

void
Polynomial::operator+= (Polynomial const &p)
{
  while (degree () < p.degree ())
    coefs_.push_back (0.0);

  for (int i = 0; i <= p.degree (); i++)
    coefs_[i] += p.coefs_[i];
}

void
Polynomial::operator-= (Polynomial const &p)
{
  while (degree () < p.degree ())
    coefs_.push_back (0.0);

  for (int i = 0; i <= p.degree (); i++)
    coefs_[i] -= p.coefs_[i];
}

void
Polynomial::scalarmultiply (Real fact)
{
  for (int i = 0; i <= degree (); i++)
    coefs_[i] *= fact;
}

void
Polynomial::set_negate (const Polynomial &src)
{
  for (int i = 0; i <= src.degree (); i++)
    coefs_[i] = -src.coefs_[i];
}

void
Polynomial::check_sol (Real x) const
{
  Real f = eval (x);
  Polynomial p (*this);
  p.differentiate ();
  Real d = p.eval (x);

  if (abs (f) > abs (d) * FUDGE)
    programming_error ("not a root of polynomial\n");
}

void
Polynomial::check_sols (vector<Real> roots) const
{
  for (vsize i = 0; i < roots.size (); i++)
    check_sol (roots[i]);
}

Polynomial::Polynomial (Real a, Real b)
{
  coefs_.push_back (a);
  if (b)
    coefs_.push_back (b);
}

/* cubic root. */
inline Real
cubic_root (Real x)
{
  if (x > 0.0)
    return pow (x, 1.0 / 3.0);
  else if (x < 0.0)
    return -pow (-x, 1.0 / 3.0);
  return 0.0;
}

static bool
iszero (Real r)
{
  return !r;
}

vector<Real>
Polynomial::solve_cubic () const
{
  vector<Real> sol;

  /* normal form: x^3 + Ax^2 + Bx + C = 0 */
  Real A = coefs_[2] / coefs_[3];
  Real B = coefs_[1] / coefs_[3];
  Real C = coefs_[0] / coefs_[3];

  /*
   * substitute x = y - A/3 to eliminate quadric term: x^3 +px + q = 0
   */

  Real sq_A = A * A;
  Real p = 1.0 / 3 * (-1.0 / 3 * sq_A + B);
  Real q = 1.0 / 2 * (2.0 / 27 * A * sq_A - 1.0 / 3 * A * B + C);

  /* use Cardano's formula */

  Real cb = p * p * p;
  Real D = q * q + cb;

  if (iszero (D))
    {
      if (iszero (q)) /* one triple solution */
        {
          sol.push_back (0);
          sol.push_back (0);
          sol.push_back (0);
        }
      else /* one single and one double solution */
        {
          Real u = cubic_root (-q);

          sol.push_back (2 * u);
          sol.push_back (-u);
        }
    }
  else if (D < 0)
    {
      /* Casus irreducibilis: three real solutions */
      Real phi = 1.0 / 3 * acos (-q / sqrt (-cb));
      Real t = 2 * sqrt (-p);

      sol.push_back (t * cos (phi));
      sol.push_back (-t * cos (phi + M_PI / 3));
      sol.push_back (-t * cos (phi - M_PI / 3));
    }
  else
    {
      /* one real solution */
      Real sqrt_D = sqrt (D);
      Real u = cubic_root (sqrt_D - q);
      Real v = -cubic_root (sqrt_D + q);

      sol.push_back (u + v);
    }

  /* resubstitute */
  Real sub = 1.0 / 3 * A;

  for (vsize i = sol.size (); i--;)
    {
      sol[i] -= sub;

#ifdef PARANOID
      assert (fabs (eval (sol[i])) < 1e-8);
#endif
    }

  return sol;
}

Real
Polynomial::lc () const
{
  return coefs_.back ();
}

Real &
Polynomial::lc ()
{
  return coefs_.back ();
}

ssize_t
Polynomial::degree () const
{
  return coefs_.size () - 1;
}
/*
  all roots of quadratic eqn.
*/
vector<Real>
Polynomial::solve_quadric () const
{
  vector<Real> sol;
  /* normal form: x^2 + px + q = 0 */
  Real p = coefs_[1] / (2 * coefs_[2]);
  Real q = coefs_[0] / coefs_[2];

  Real D = p * p - q;

  if (D > 0)
    {
      D = sqrt (D);

      sol.push_back (D - p);
      sol.push_back (-D - p);
    }
  return sol;
}

/* solve linear equation */
vector<Real>
Polynomial::solve_linear () const
{
  vector<Real> s;
  if (coefs_[1])
    s.push_back (-coefs_[0] / coefs_[1]);
  return s;
}

vector<Real>
Polynomial::solve () const
{
  Polynomial *me = (Polynomial *)this;
  me->clean ();

  switch (degree ())
    {
    case 1:
      return solve_linear ();
    case 2:
      return solve_quadric ();
    case 3:
      return solve_cubic ();
    }
  vector<Real> s;
  return s;
}

void
Polynomial::operator*= (Polynomial const &p2)
{
  *this = multiply (*this, p2);
}
