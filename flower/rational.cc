/*
  rational.cc -- implement Rational

  source file of the Flower Library

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "rational.hh"

#include <cmath>
#include <cassert>
#include <cstdlib>
using namespace std;

#include "string-convert.hh"
#include "libc-extension.hh"

double
Rational::to_double () const
{
  if (sign_ == -1 || sign_ == 1 || sign_ == 0)
    return ((double)sign_) * num_ / den_;
  if (sign_ == -2)
    return -HUGE_VAL;
  else if (sign_ == 2)
    return HUGE_VAL;
  else
    assert (false);

  return 0.0;
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
  if (is_infinity())
    return *this;
  return Rational ((num_ - (num_ % den_)) * sign_, den_);
}

Rational::Rational ()
{
  sign_ = 0;
  num_ = den_ = 1;
}

Rational::Rational (I64 n, I64 d)
{
  sign_ = ::sign (n) * ::sign (d);
  num_ = ::abs (n);
  den_ = ::abs (d);
  normalize ();
}

Rational::Rational (I64 n)
{
  sign_ = ::sign (n);
  num_ = ::abs (n);
  den_ = 1;
}

Rational::Rational (int n)
{
  sign_ = ::sign (n);
  num_ = ::abs (n);
  den_ = 1;
}


void
Rational::set_infinite (int s)
{
  sign_ = ::sign (s) * 2;
  num_ = 1;
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


/*
  copy & paste from scm_gcd (GUILE).
 */
static I64
gcd (I64 u, I64 v)
{
  I64 result = 0;
  if (u == 0)
    result = v;
  else if (v == 0)
    result = u;
  else
    {
      I64 k = 1;
      I64 t;
      /* Determine a common factor 2^k */
      while (!(1 & (u | v)))
	{
	  k <<= 1;
	  u >>= 1;
	  v >>= 1;
	}
      /* Now, any factor 2^n can be eliminated */
      if (u & 1)
	t = -v;
      else
	{
	  t = u;
	b3:
	  t = t >> 1;
	}
      if (!(1 & t))
	goto b3;
      if (t > 0)
	u = t;
      else
	v = -t;
      t = u - v;
      if (t != 0)
	goto b3;
      result = u * k;
    }

  return result;
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
      I64 g = gcd (num_, den_);

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
  return r.sign_ * ::sign ((I64) (r.num_ * s.den_) - (I64) (s.num_ * r.den_));
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
      I64 lcm = (den_ / gcd (r.den_, den_)) * r.den_;
      I64 n = sign_ * num_ * (lcm / den_) + r.sign_ * r.num_ * (lcm / r.den_);
      I64 d = lcm;
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

      num_ = (U64) (mantissa * FACT);
      den_ = (U64) FACT;
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
  I64 r (num_);
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
