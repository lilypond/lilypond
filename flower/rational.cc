/*
  rational.cc -- implement Rational

  source file of the Flower Library

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "rational.hh"

#include <cmath>
#include <cstdlib>
using namespace std;

#include "string-convert.hh"
#include "libc-extension.hh"

double
Rational::to_double () const
{
  return ((double)sign_) * num_ / den_;
}


#ifdef STREAM_SUPPORT
ostream &
operator << (ostream &o, Rational r)
{
  o << r.string ();
  return o;
}
#endif

Rational
Rational::abs () const
{
  return Rational (num_, den_);
}

Rational
Rational::trunc_rat () const
{
  return Rational (num_ - (num_ % den_), den_);
}

Rational::Rational ()
{
  sign_ = 0;
  num_ = den_ = 1;
}

Rational::Rational (int n, int d)
{
  sign_ = ::sign (n) * ::sign (d);
  num_ = ::abs (n);
  den_ = ::abs (d);
  normalize ();
}

Rational::Rational (int n)
{
  sign_ = ::sign (n);
  num_ = ::abs (n);
  den_ = 1;
}


/*
  We can actually do a little better. See Knuth 4.5.2
 */
static inline
int gcd (int a, int b)
{
  int t;
  while ((t = a % b))
    {
      a = b;
      b = t;
    }
  return b;
}

void
Rational::set_infinite (int s)
{
  sign_ = ::sign (s) * 2;
}

Rational
Rational::operator - () const
{
  Rational r (*this);
  r.negate ();
  return r;
}

Rational
Rational::div_rat (Rational div) const
{
  Rational r (*this);
  r /= div;
  return r.trunc_rat ();
}

Rational
Rational::mod_rat (Rational div) const
{
  Rational r (*this);
  r = (r / div - r.div_rat (div)) * div;
  return r;
}

void
Rational::normalize ()
{
  if (!sign_)
    {
      den_ = 1;
      num_ = 0;
    }
  else if (!den_)
    {
      sign_ = 2;
      num_ = 1;
    }
  else if (!num_)
    {
      sign_ = 0;
      den_ = 1;
    }
  else
    {
      int g = gcd (num_, den_);

      num_ /= g;
      den_ /= g;
    }
}
int
Rational::sign () const
{
  return ::sign (sign_);
}

int
Rational::compare (Rational const &r, Rational const &s)
{
  if (r.sign_ < s.sign_)
    return -1;
  else if (r.sign_ > s.sign_)
    return 1;
  else if (r.is_infinity ())
    return 0;
  else if (r.sign_ == 0)
    return 0;
  return r.sign_ * ::sign (int (r.num_ * s.den_) - int (s.num_ * r.den_));
}

int
compare (Rational const &r, Rational const &s)
{
  return Rational::compare (r, s);
}

Rational &
Rational::operator %= (Rational r)
{
  *this = mod_rat (r);
  return *this;
}

Rational &
Rational::operator += (Rational r)
{
  if (is_infinity ())
    ;
  else if (r.is_infinity ())
    *this = r;
  else
    {
      int lcm = (den_ / gcd (r.den_, den_)) * r.den_;
      int n = sign_ * num_ * (lcm / den_) + r.sign_ * r.num_ * (lcm / r.den_);
      int d = lcm;
      sign_ = ::sign (n) * ::sign (d);
      num_ = ::abs (n);
      den_ = ::abs (d);
      normalize ();
    }
  return *this;
}

/*
  copied from libg++ 2.8.0
*/
Rational::Rational (double x)
{
  if (x != 0.0)
    {
      sign_ = ::sign (x);
      x *= sign_;

      int expt;
      double mantissa = frexp (x, &expt);

      const int FACT = 1 << 20;

      /*
	Thanks to Afie for this too simple  idea.

	do not blindly substitute by libg++ code, since that uses
	arbitrary-size integers.  The rationals would overflow too
	easily.
      */

      num_ = (unsigned int) (mantissa * FACT);
      den_ = (unsigned int) FACT;
      normalize ();
      if (expt < 0)
	den_ <<= -expt;
      else
	num_ <<= expt;
      normalize ();
    }
  else
    {
      num_ = 0;
      den_ = 1;
      sign_ = 0;
      normalize ();
    }
}

void
Rational::invert ()
{
  int r (num_);
  num_ = den_;
  den_ = r;
}

Rational &
Rational::operator *= (Rational r)
{
  sign_ *= ::sign (r.sign_);
  if (r.is_infinity ())
    {
      sign_ = sign () * 2;
      goto exit_func;
    }

  num_ *= r.num_;
  den_ *= r.den_;

  normalize ();
 exit_func:
  return *this;
}

Rational &
Rational::operator /= (Rational r)
{
  r.invert ();
  return (*this *= r);
}

void
Rational::negate ()
{
  sign_ *= -1;
}

Rational &
Rational::operator -= (Rational r)
{
  r.negate ();
  return (*this += r);
}

string
Rational::to_string () const
{
  if (is_infinity ())
    {
      string s (sign_ > 0 ? "" : "-");
      return string (s + "infinity");
    }

  string s = ::to_string (num ());
  if (den () != 1 && num ())
    s += "/" + ::to_string (den ());
  return s;
}

int
Rational::to_int () const
{
  return (int) num () / den ();
}

int
sign (Rational r)
{
  return r.sign ();
}

bool
Rational::is_infinity () const
{
  return sign_ == 2 || sign_ == -2;
}
