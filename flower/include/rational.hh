/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "compare.hh"
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

private:
  // n.b. can be used to intialize abnormally
  constexpr Rational (int sign, U64 num, U64 den)
    : sign_ (sign), num_ (num), den_ (den)
  {
  }

  void normalize ();

public:
  void invert ();
  I64 numerator () const { return sign_ * static_cast<I64> (num_); }
  I64 denominator () const { return static_cast<I64> (den_); }
  I64 num () const { return numerator (); }
  I64 den () const { return denominator (); }

  // n.b. not valid for infinities
  I64 trunc_int () const;
  Rational trunc_rat () const;
  Rational div_rat (Rational) const;
  Rational mod_rat (Rational) const;
  Rational abs () const;
  void negate ();

  // TODO: Returning true for 0/0 would be analogous to floating-point types,
  // but changing it might have significant side effects.
  explicit operator bool () const { return sign_ != 0; }
  explicit operator double () const;

  constexpr Rational operator - () const { return { -sign_, num_, den_ }; }

  // default to zero
  constexpr Rational () : sign_ (0), num_ (1), den_ (1) {}

  // positive infinity
  static constexpr Rational infinity () { return { 2, 1, 1 }; }

  // Allow implicit conversion from integer.  All of these must be defined or
  // deleted to avoid ambiguity.  "long long" is specified by the C++ standard
  // to be at least 64 bits wide, which is what we are storing.
  Rational (int n) : Rational (static_cast<long long> (n)) {}
  Rational (long n) : Rational (static_cast<long long> (n)) {}
  Rational (long long n);
  Rational (unsigned n) : Rational (static_cast<unsigned long long> (n)) {}
  Rational (unsigned long n) : Rational (static_cast<unsigned long long> (n)) {}
  Rational (unsigned long long);

  explicit Rational (I64, I64);
  explicit Rational (double);
  Rational (Rational const &r) = default;
  Rational &operator = (Rational const &r) = default;

  Rational &operator *= (Rational);
  Rational &operator /= (Rational);
  Rational &operator += (Rational);
  Rational &operator -= (Rational);
  Rational &operator %= (Rational);
  static int compare (Rational const &, Rational const &);
  int sign () const;
  std::string to_string () const;

  // true for positive or negative infinity
  // TODO: Consider isfinite(const Rational&) would likely make more sense in
  // some cases, even though Rational doesn't properly represent NaN yet.
  friend bool isinf (Rational const &r) { return r.sign_ / 2; }
};

#include "arithmetic-operator.hh"

IMPLEMENT_ARITHMETIC_OPERATOR (Rational, / );
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, +);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, *);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, -);
IMPLEMENT_ARITHMETIC_OPERATOR (Rational, % );

INSTANTIATE_COMPARE (Rational const &, Rational::compare);

int compare (Rational const &, Rational const &);
int sign (Rational r);

#if 0
ostream &
operator << (ostream &, Rational);
#endif

inline std::string
to_string (Rational const &r)
{
  return r.to_string ();
}

#endif // RATIONAL_HH
