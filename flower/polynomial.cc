/*
   poly.cc -- routines for manipulation of polynomials in one var

   (c) 1993--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include <math.h>

#include "polynomial.hh"

/*
   Een beter milieu begint bij uzelf. Hergebruik!


   This was ripped from Rayce, a raytracer I once wrote. 
*/

Real
Polynomial::eval (Real x)const
{
  Real p = 0.0;

  // horner's scheme
  for (int i = coefs_.size (); i--; )
    p = x * p + coefs_[i];
  
  return p;
}


Polynomial
Polynomial::multiply(const Polynomial & p1, const Polynomial & p2)
{
  Polynomial dest;

  int deg= p1.degree () + p2.degree ();
  for (int i = 0; i <= deg; i++)
    {
      dest.coefs_.push (0);
      for (int j = 0; j <= i; j++)
	if (i - j <= p2.degree () && j <= p1.degree ())
	  dest.coefs_.top () += p1.coefs_[j] * p2.coefs_[i - j];
    }
    
  return dest;
}

void
Polynomial::differentiate()
{
  for (int i = 1; i<= degree (); i++)
    {
      coefs_[i-1] = coefs_[i] * i;
    }
  coefs_.pop ();
}

Polynomial
Polynomial::power(int exponent, const Polynomial & src)
{
  int e = exponent;
  Polynomial dest(1), base(src);
    
  // classicint power. invariant: src^exponent = dest * src ^ e
  // greetings go out to Lex Bijlsma & Jaap vd Woude
  while (e > 0)
    {
      if (e % 2)
        {
	  dest = multiply(dest, base);
	  e--;
        } else
	  {
            base = multiply(base, base);
            e /= 2;
	  }
    }
  return  dest;
}

static Real const FUDGE = 1e-8;

void
Polynomial::clean()
{
  int i;
  for (i = 0; i <= degree (); i++)
    {
      if (abs(coefs_[i]) < FUDGE)
	coefs_[i] = 0.0;
    }

  while (degree () > 0 &&
	 (fabs (coefs_.top ()) < FUDGE * fabs (coefs_.top (1)))
	 || !coefs_.top ())
    coefs_.pop ();
}


Polynomial 
Polynomial::add(const Polynomial & p1, const Polynomial & p2)
{
  Polynomial dest;
  int tempord =  p2.degree () >? p1.degree ();
  for (int i = 0; i <= tempord; i++)
    {
      Real temp = 0.0;
      if (i <= p1.degree ())
	temp += p1.coefs_[i];
      if (i <= p2.degree ())
	temp += p2.coefs_[i];
      dest.coefs_.push (temp);
    }
  return dest;
}

void
Polynomial::scalarmultiply(Real fact)
{
  for (int i = 0; i <= degree (); i++)
    coefs_[i] *= fact;
}

Polynomial
Polynomial::subtract(const Polynomial & p1, const Polynomial & p2)
{
  Polynomial dest;
  int tempord =  p2.degree () >? p1.degree ();
  
  for (int i = 0; i <= tempord; i++)
    {
      Real temp = 0.0;	// can't store result directly.. a=a-b
      if (i <= p1.degree ())
	temp += p1.coefs_[i];
      if (i <= p2.degree ())
	temp -= p2.coefs_[i];
      dest.coefs_.push (temp);
    }
  return dest;
  
}

void
Polynomial::set_negate(const Polynomial & src)
{
  for (int i = 0; i <= src.degree(); i++)
    coefs_[i] = -src.coefs_[i];
}

/// mod of #u/v#
int 
Polynomial::set_mod(const Polynomial &u, const Polynomial &v)
{
  (*this) = u;
    
  if (v.lc() < 0.0) {
    for (int k = u.degree () - v.degree () - 1; k >= 0; k -= 2)
      coefs_[k] = -coefs_[k];

    for (int k = u.degree () - v.degree (); k >= 0; k--)
      for (int j = v.degree () + k - 1; j >= k; j--)
	coefs_[j] = -coefs_[j] - coefs_[v.degree () + k] * v.coefs_[j - k];
  } else {
    for (int k = u.degree () - v.degree (); k >= 0; k--)
      for (int j = v.degree () + k - 1; j >= k; j--)
	coefs_[j] -= coefs_[v.degree () + k] * v.coefs_[j - k];
  }

  int k = v.degree () - 1;
  while (k >= 0 && coefs_[k] == 0.0)
    k--;

  coefs_.set_size(1+ ( (k < 0) ? 0 : k));
  return degree();
}

void
Polynomial::check_sol(Real x) const 
{
  Real f=eval(x);
  Polynomial p(*this);
  p.differentiate();
  Real d = p.eval(x);
  
  if( abs(f) > abs(d) * FUDGE)
    ;
  /*
    warning("x=%f is not a root of polynomial\n"
    "f(x)=%f, f'(x)=%f \n", x, f, d);	*/
}
	
void
Polynomial::check_sols(Array<Real> roots) const
{
  for (int i=0; i< roots.size (); i++)
    check_sol(roots[i]);
}

Polynomial::Polynomial (Real a, Real b)
{
  coefs_.push (a);
  if (b)
    coefs_.push (b);
}

/* cubic root. */
inline Real cubic_root(Real x)
{
  if (x > 0.0)
    return pow(x, 1.0/3.0) ;
  else if (x < 0.0)
    return -pow(-x, 1.0/3.0);
  else
    return 0.0;
}

static bool
iszero (Real r)
{
  return !r;
}

Array<Real>
Polynomial::solve_cubic()const
{
  Array<Real> sol;
  
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

  Real cb_p = p * p * p;
  Real D = q * q + cb_p;

  if (iszero(D)) {
    if (iszero(q)) {	/* one triple solution */
      sol.push (0);
      sol.push (0);
      sol.push (0);      
    } else {		/* one single and one double solution */
      Real u = cubic_root(-q);

      sol.push (2 * u);
      sol.push (-u);
    }
  } else if (D < 0) {		/* Casus irreducibilis: three real solutions */
    Real phi = 1.0 / 3 * acos(-q / sqrt(-cb_p));
    Real t = 2 * sqrt(-p);

    sol.push (t * cos(phi));
    sol.push (-t * cos(phi + M_PI / 3));
    sol.push ( -t * cos(phi - M_PI / 3));
  } else {			/* one real solution */
    Real sqrt_D = sqrt(D);
    Real u = cubic_root(sqrt_D - q);
    Real v = -cubic_root(sqrt_D + q);

    sol.push ( u + v);
  }

  /* resubstitute */
  Real sub = 1.0 / 3 * A;

  for (int i = sol.size (); i--;)
    {
      sol[i] -= sub;

      assert (fabs (eval (sol[i]) ) < 1e-8);
    }
  
  return sol;
}

Real
Polynomial::lc () const
{
  return coefs_.top();
}

Real&
Polynomial::lc () 
{
  return coefs_.top ();
}

int
Polynomial::degree ()const
{
  return coefs_.size () -1;
}
/*
   all roots of quadratic eqn.
 */
Array<Real>
Polynomial::solve_quadric()const
{
  Array<Real> sol;
  /* normal form: x^2 + px + q = 0 */
  Real p = coefs_[1] / (2 * coefs_[2]);
  Real q = coefs_[0] / coefs_[2];

  Real D = p * p - q;

  if (D>0) {
    D = sqrt(D);

    sol.push ( D - p);
    sol.push ( -D - p);
  }
  return sol; 
}

/* solve linear equation */
Array<Real>
Polynomial::solve_linear()const
{
  Array<Real> s;
  if (coefs_[1])
    s.push ( -coefs_[0] / coefs_[1]);
  return s;
}


Array<Real>
Polynomial::solve () const
{
  Polynomial * me = (Polynomial*) this;
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
  assert (false);
  Array<Real> s;
  return s;
}

void
Polynomial:: operator *= (Polynomial const &p2)
{
  *this = multiply (*this,p2);
}  

void
Polynomial::operator += (Polynomial const &p)
{
  *this = add( *this, p);
}

void
Polynomial::operator -= (Polynomial const &p)
{
  *this = subtract(*this, p);
}
