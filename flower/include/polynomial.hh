
/*
 * poly.h -- routines for manipulation of polynomials in one var
 *
 * (c) 1993--2008 Han-Wen Nienhuys
 */

#ifndef POLY_H
#define POLY_H

#include "std-vector.hh"
#include "arithmetic-operator.hh"
#include "real.hh"

/// structure for a polynomial in one var. 
struct Polynomial
{
  /// degree of polynomial
  int degree ()const;

  /// coefficients 
  vector<Real> coefs_;

  // leading coef
  Real &lc ();

  // leading coef
  Real lc () const;
  void print () const;
  Real eval (Real) const;
  void print_sols (vector<Real>) const;
  void check_sols (vector<Real>) const;
  void check_sol (Real x) const;
  static Polynomial multiply (const Polynomial &p1, const Polynomial &p2);
  static Polynomial power (int exponent, const Polynomial &src);

  /// chop low coefficients
  void clean ();

  /// eliminate #x#  close to  zero
  void real_clean ();
  void scalarmultiply (Real fact);
  void operator *= (Real f) { scalarmultiply (f); }
  void operator /= (Real f) { scalarmultiply (1 / f); }
  void operator += (Polynomial const &p2);
  void operator *= (Polynomial const &p2);
  void operator -= (Polynomial const &p2);
  Polynomial (Real a, Real b = 0.0);
  Polynomial (){}
  void set_negate (const Polynomial &src);

  /// take the derivative
  void differentiate ();
  int set_mod (const Polynomial &u, const Polynomial &v);

  void debug_clean ();

  vector<Real> solve_quadric ()const;
  vector<Real> solve_cubic ()const;
  vector<Real> solve_linear ()const;

  vector<Real> solve () const;
};

IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, -);
IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, +);
IMPLEMENT_ARITHMETIC_OPERATOR (Polynomial, *);

inline Polynomial
operator * (Polynomial p, Real a)
{
  p *= a;
  return p;
}
inline Polynomial
operator * (Real a, Polynomial p)
{
  p *= a;
  return p;
}
#endif

