/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "std-string.hh"

#include <cstdint>
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
  uint64_t num_;
  uint64_t den_;

private:
  // n.b. can be used to intialize abnormally
  constexpr Rational (int sign, uint64_t num, uint64_t den)
    : sign_ (sign),
      num_ (num),
      den_ (den)
  {
  }

  void normalize ();

public:
  int64_t numerator () const { return sign_ * static_cast<int64_t> (num_); }
  int64_t denominator () const { return static_cast<int64_t> (den_); }
  int64_t num () const { return numerator (); }
  int64_t den () const { return denominator (); }

  // n.b. not valid for infinities
  int64_t trunc_int () const;
  Rational trunc_rat () const;
  Rational div_rat (Rational) const;
  Rational mod_rat (Rational) const;
  Rational abs () const;
  void negate ();

  explicit operator bool () const { return sign_ != 0; }
  explicit operator double () const;

  constexpr Rational operator- () const { return {-sign_, num_, den_}; }

  // default to zero
  constexpr Rational ()
    : sign_ (0),
      num_ (1),
      den_ (1)
  {
  }

  // positive infinity
  static constexpr Rational infinity () { return {2, 1, 1}; }

  // not-a-number
  static constexpr Rational nan () { return {1, 0, 0}; }

  // Allow implicit conversion from integer.  All of these must be defined or
  // deleted to avoid ambiguity.  "long long" is specified by the C++ standard
  // to be at least 64 bits wide, which is what we are storing.
  Rational (int n)
    : Rational (static_cast<long long> (n))
  {
  }
  Rational (long n)
    : Rational (static_cast<long long> (n))
  {
  }
  Rational (long long n);
  Rational (unsigned n)
    : Rational (static_cast<unsigned long long> (n))
  {
  }
  Rational (unsigned long n)
    : Rational (static_cast<unsigned long long> (n))
  {
  }
  Rational (unsigned long long);

  // n.b. {0, 0} is treated as zero rather than NaN
  explicit Rational (int64_t, int64_t);

  explicit Rational (double);
  Rational (Rational const &r) = default;
  Rational &operator= (Rational const &r) = default;

  Rational &operator*= (Rational);
  Rational &operator/= (Rational);
  Rational &operator+= (Rational);
  Rational &operator-= (Rational);
  Rational &operator%= (Rational);
  static int compare (Rational const &, Rational const &);
  int sign () const;
  std::string to_string () const;

  // false for positive infinity, negative infinity, or not-a-number
  friend bool isfinite (Rational const &r) { return r.den_ && !isinf (r); }

  // true for positive or negative infinity
  friend bool isinf (Rational const &r) { return r.sign_ / 2; }

  // true for not-a-number
  friend bool isnan (Rational const &r) { return !r.den_; }
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

inline std::string
to_string (Rational const &r)
{
  return r.to_string ();
}

#endif // RATIONAL_HH
