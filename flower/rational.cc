/*
  rational.cc -- implement Rational
  
  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>
#include <stdlib.h>
#include "rational.hh"
#include "string.hh"
#include "string-convert.hh"  
#include "libc-extension.hh"

Rational::operator double () const
{
  return (double)sign_ * num_ / den_;
}

#ifdef STREAM_SUPPORT
ostream &
operator << (ostream &o, Rational r)
{
  o <<  r.str ();
  return o;
}
#endif


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
  num_ = abs (n);
  den_ = abs (d);
  normalise ();
}

Rational::Rational (int n)
{
  sign_ = ::sign (n);
  num_ = abs (n);
  den_= 1;
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
Rational::normalise ()
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
      int g = gcd (num_ , den_);

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
  else if (r.infty_b ())
    return 0;
  else if (r.sign_ == 0)
    return 0;
  else
    {
      return r.sign_ * ::sign  (int (r.num_ * s.den_) - int (s.num_ * r.den_));
    }
}

int
compare (Rational const &r, Rational const &s)
{
  return Rational::compare (r, s );
}

Rational &
Rational::operator %= (Rational r)
{
  *this = r.mod_rat (r);
  return *this;
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
      sign_ =  ::sign (n) * ::sign (d);
      num_ = abs (n);
      den_ = abs (d);
      normalise ();
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
      normalise ();      
      if (expt < 0)
	den_ <<= -expt;
      else
	num_ <<= expt;
      normalise ();
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
Rational::to_int () const
{
  return num () / den ();
}

int
sign (Rational r)
{
  return r.sign ();
}
