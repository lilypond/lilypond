/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef RATIONAL_HH
#define RATIONAL_HH

#include "flower-proto.hh"
#include "std-string.hh"
#include <limits.h>

/**
   Rational numbers.  Included is support for + and - infinity.
*/
class Rational
{
  /**
     Sign of rational.
     -2, .. 2

     -2,2 is - and + infinity.
     -1,1 is negative and positive.
     0 if *this is zero.
  */
  int sign_;
  U64 num_, den_;
  void normalize ();
  void copy (Rational const &);

public:
  void set_infinite (int sign);
  bool is_infinity () const;
  void invert ();
  I64 numerator () const { return sign_ * num_; }
  I64 denominator () const { return den_; }
  I64 num () const { return numerator (); }
  I64 den () const { return denominator (); }

  Rational trunc_rat () const;
  Rational div_rat (Rational) const;
  Rational mod_rat (Rational) const;
  Rational abs () const;
  void negate ();
  int to_int () const;

  operator double () const { return to_double (); }
  double to_double () const;

  Rational operator - () const;
  /**
     Initialize to 0.
  */
  Rational ();
  Rational (int);
  Rational (I64);
  Rational (U64);
  explicit Rational (I64, I64);
  explicit Rational (double);
  Rational (Rational const &r) { copy (r);}
  Rational &operator = (Rational const &r)
  {
    copy (r); return *this;
  }

  Rational &operator *= (Rational);
  Rational &operator /= (Rational);
  Rational &operator += (Rational);
  Rational &operator -= (Rational);
  Rational &operator %= (Rational);
  static int compare (Rational const &, Rational const &);
  int sign () const;
  string to_string () const;
};

#include "arithmetic-operator.hh"

IMPLEMENT_ARITHMETIC_OPERATOR (Rational, /);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, +);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, *);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, -);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, %);

INSTANTIATE_COMPARE (Rational const &, Rational::compare);

int compare (Rational const &, Rational const &);
int sign (Rational r);

inline void
Rational::copy (Rational const &r)
{
  sign_ = r.sign_;
  num_ = r.num_;
  den_ = r.den_;
}

#if 0
ostream &
operator << (ostream &, Rational);
#endif

const Rational infinity_rat (U64_MAX);

#endif // RATIONAL_HH
