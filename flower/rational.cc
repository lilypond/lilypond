/*
  rational.cc -- implement Rational
  
  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <stdlib.h>
#include "rational.hh"
#include "string.hh"
#include "string-convert.hh"  
#include "libc-extension.hh"

Rational::operator bool () const
{
  return sign_;
}

Rational::operator int () const
{
  return sign_ * num_ / den_;
}

Rational::operator double () const
{
  return (double)sign_ * num_ / den_;
}

ostream &
operator << (ostream &o, Rational r)
{
  o <<  r.str ();
  return o;
}

Rational
Rational::truncated () const
{
  return Rational(num_ - (num_ % den_), den_);
}

Rational::Rational ()
{
  sign_ = 0;
  num_ = den_ = 1;
}

Rational::Rational (int n, int d)
{
  sign_ = ::sign (n) * ::sign (d);
  num_ = abs (n);
  den_ = abs (d);
  normalise ();
}

Rational::Rational (Rational const &r)
{
  copy (r);
}

static
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

static
int lcm (int a, int b)
{
  return abs (a*b / gcd (a,b));
}

void
Rational::set_infinite (int s)
{
  sign_ = ::sign (s) * 2; 
}

Rational
Rational::operator - () const
{
  Rational r(*this);
  r.negate ();
  return r;
}

void
Rational::normalise ()
{
  if (!sign_)
    {
      den_ = 1;
      num_ = 0;
      return ;
    }
  if (!den_)
    sign_ = 2;
  if (!num_)
    sign_ = 0;

  int g = gcd (num_ , den_);

  num_ /= g;
  den_ /= g;
}

int
Rational::sign () const
{
  return ::sign (sign_);
}

bool
Rational::infty_b () const
{
  return abs (sign_) > 1;
}

int
Rational::compare (Rational const &r, Rational const &s)
{
  if (r.sign_ < s.sign_)
    return -1;
  else if (r.sign_ > s.sign_)
    return 1;
  else if (r.infty_b ())
    return 0;

  return  (r - s).sign ();
}

int
compare (Rational const &r, Rational const &s)
{
  return Rational::compare (r, s );
}

Rational &
Rational::operator += (Rational r)
{
  if (infty_b ())
    ;
  else if (r.infty_b ())
    {
      *this = r;
    }
  else 
    {
      int n = sign_ * num_ *r.den_ + r.sign_ * den_ * r.num_;
      int d = den_ * r.den_;
      sign_ =  ::sign (n) * ::sign(d);
      num_ = abs (n);
      den_ = abs (d);
      normalise ();
    }
  return *this;
}
    

/*
  copied from libg++ 2.8.0
 */ 
Rational::Rational(double x)
{
  if (x != 0.0)
    {
      sign_ = ::sign (x);
      x *= sign_;

      int expt;
      double mantissa = frexp(x, &expt);

      const int FACT = 1 << 20;

      /*
	FIXME

	SHOULD TEST THIS
	
	suck me gently with a vacuum cleaner.  Thanks to Afie for this wierd idea.
       */
      num_ = (unsigned int) (mantissa * FACT);
      den_ = (unsigned int) FACT;
      normalise ();
      num_ <<= expt;
    }
  else
    {
      num_ = 0;
      den_ = 1;
      sign_ =0;
      normalise ();
    }
}


void
Rational::invert ()
{
  int r (num_);
  num_  = den_;
  den_ = r;
}

Rational &
Rational::operator *= (Rational r)
{
  sign_ *= ::sign (r.sign_);
  if (r.infty_b ())
    {	
      sign_ = sign () * 2;
      goto exit_func;
    }

  num_ *= r.num_;
  den_ *= r.den_;

  normalise ();
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

Rational&
Rational::operator -= (Rational r)
{
  r.negate ();
  return (*this += r);
}

/*
  be paranoid about overiding libg++ stuff
 */
Rational &
Rational::operator = (Rational const &r)
{
  copy (r);
  return *this;
}

String
Rational::str () const
{
  if (infty_b ())
    {
      String s (sign_ > 0 ? "" : "-" );
      return String (s + "infinity");
    }
  String s = to_str (num ());
  if (den () != 1 && num ())
    s += "/" + to_str (den ());
  return s;
}

int
sign (Rational r)
{
  return r.sign ();
}
