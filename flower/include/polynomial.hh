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

/*
 * polynomial.hh -- routines for manipulation of polynomials in one var
 */

#ifndef POLY_H
#define POLY_H

#include "arithmetic-operator.hh"
#include "real.hh"
#include "std-vector.hh"
#include <sys/types.h>

/// structure for a polynomial in one var.
struct Polynomial
{
  /// degree of polynomial
  ssize_t degree () const;

  /// coefficients
  std::vector<Real> coefs_;

  // leading coef
  Real &lc ();

  // leading coef
  Real lc () const;
  void print () const;
  Real eval (Real) const;
  Real minmax (Real, Real, bool) const;
  void print_sols (std::vector<Real>) const;
  void check_sols (std::vector<Real>) const;
  void check_sol (Real x) const;
  static Polynomial multiply (const Polynomial &p1, const Polynomial &p2);
  static Polynomial power (int exponent, const Polynomial &src);

  /// chop low coefficients
  void clean ();

  /// eliminate #x#  close to  zero
  void real_clean ();
  void scalarmultiply (Real fact);
  void operator*= (Real f) { scalarmultiply (f); }
  void operator/= (Real f) { scalarmultiply (1 / f); }
  void operator+= (Polynomial const &p2);
  void operator*= (Polynomial const &p2);
  void operator-= (Polynomial const &p2);
  Polynomial (Real a, Real b = 0.0);
  Polynomial () {}
  void set_negate (const Polynomial &src);

  /// take the derivative
  void differentiate ();

  void debug_clean ();

  std::vector<Real> solve_quadric () const;
  std::vector<Real> solve_cubic () const;
  std::vector<Real> solve_linear () const;

  std::vector<Real> solve () const;
};

IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, -);
IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, +);
IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, *);

inline Polynomial operator* (Polynomial p, Real a)
{
  p *= a;
  return p;
}
inline Polynomial operator* (Real a, Polynomial p)
{
  p *= a;
  return p;
}
#endif
