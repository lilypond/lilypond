/*
  rational.hh -- declare rational helpers

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef RATIONAL_HH
#define RATIONAL_HH

#if PARANOIA
#ifndef Rational
#define Rational MyRational
#endif
#endif

#include "compare.hh"
#include "arithmetic-operator.hh"
#include "fproto.hh"

/**
   Rational numbers.  Included is support for + and - infinity.
 */
class Rational {
  /**
     Sign of rational.
     -2, .. 2

     -2,2 is - and + infinity.
     -1,1 is negative and positive.
     0 if *this is zero.
   */
  int sign_;
  unsigned int num_, den_;
  void normalise ();
  void copy (Rational const &);

public:
  void set_infinite (int sign);
  bool infty_b () const;
  void invert ();
  int num  () const { return sign_ * num_; }
  int den  () const { return den_; }
  int num_i  () const { return sign_ * num_; }
  int den_i  () const { return den_; }
  Rational truncated () const;
  void negate ();
  operator bool () const;
  operator int () const;
  operator double () const;
  Rational operator - () const;
  /**
     Initialize to 0. 
   */
  Rational ();
  Rational (int, int =1);
  Rational (double);
  Rational (Rational const&);

  Rational &operator = (Rational const &);
  Rational &operator *= (Rational);
  Rational &operator /= (Rational);  
  Rational &operator += (Rational);
  Rational &operator -= (Rational);
  static int compare (Rational const&, Rational const&);
  int sign () const;
  String str () const;
};

IMPLEMENT_ARITHMETIC_OPERATOR (Rational, / );
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, + );
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, * );
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, - );

INSTANTIATE_COMPARE (Rational const&, Rational::compare);

int compare (Rational const&,Rational const&);
int sign (Rational r);

inline void
Rational::copy (Rational const&r)
{
  sign_ = r.sign_;
  num_ = r.num_;
  den_ = r.den_;
}

class ostream;
ostream &
operator << (ostream &,  Rational);

#endif // RATIONAL_HH
