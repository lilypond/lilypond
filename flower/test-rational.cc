/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020 Daniel Eble <dan@faithful.be>

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

#include "rational.hh"

#include "yaffut.hh"

#include <cmath>
#include <cstdint>
#include <limits>
#include <type_traits>

class Rational_test
{
};

TEST (Rational_test, init_default)
{
  Rational r;
  EQUAL (0, r.sign ());
  EQUAL (0, r.num ());
  EQUAL (1, r.den ());
}

template <typename T>
class Rational_int_test
{
protected:
  void test_init ()
  {
    // zero, explicit construction
    {
      const Rational r (T (0));
      EQUAL (0, r.sign ());
      EQUAL (0, r.num ());
      EQUAL (1, r.den ());
    }

    // lowest value, implicit construction
    {
      const auto L = std::numeric_limits<T>::lowest ();
      Rational r (789);
      r = L; // construct Rational (L) and copy-assign
      EQUAL (L ? -1 : 0, r.sign ());
      EQUAL (static_cast<decltype (r.num ())> (L), r.num ());
      EQUAL (1, r.den ());
    }

    // highest value, explicit construction
    {
      const auto H = std::numeric_limits<T>::max ();
      const Rational r (H);
      EQUAL (1, r.sign ());
      EQUAL (static_cast<decltype (r.num ())> (H), r.num ());
      EQUAL (1, r.den ());
    }
  }
};

#if 0 // TODO: fix initialization from lowest ()
TEST (Rational_int_test<int>, init_int)
{
  test_init ();
}
#endif

#if 0 // TODO: fix ambiguous overloading
TEST (Rational_int_test<long>, init_long)
{
  test_init ();
}
#endif

TEST (Rational_int_test<long long>, init_ll)
{
  test_init ();
}

#if 0 // TODO: fix ambiguous overloading
TEST (Rational_int_test<unsigned>, init_unsigned)
{
  test_init ();
}
#endif

#if 0 // TODO: fix ambiguous overloading
TEST (Rational_int_test<unsigned long>, init_ul)
{
  test_init ();
}
#endif

#if 0 // TODO: debug constructor: it gets the sign wrong
TEST (Rational_int_test<unsigned long long>, init_ull)
{
  test_init ();
}
#endif

// C++ can interpret bare 0 as a pointer, so test it separately.
TEST (Rational_test, assign_literal_zero)
{
  Rational r (789);
  r = 0;
  EQUAL (0, r.sign ());
  EQUAL (0, r.num ());
  EQUAL (1, r.den ());
}

TEST (Rational_test, init_zero_over_zero)
{
  const Rational r (0, 0);
  EQUAL (0, r.sign ());
  CHECK (!r.is_infinity ());
  if (false) // TODO: Rational (0, 0) -> NaN
    {
      CHECK (std::isnan (static_cast<double> (r))); // TODO: etc.
    }
}

#if 0 // TODO: Rational (positive, 0) -> +infinity
TEST (Rational_test, init_pos_over_zero)
{
  const Rational r (123, 0);
  EQUAL (1, r.sign ());
  CHECK (r.is_infinity ());
}
#endif

#if 0 // TODO: Rational (negative, 0) -> -infinity
TEST (Rational_test, init_neg_over_zero)
{
  const Rational r (-123, 0);
  EQUAL (-1, r.sign ());
  CHECK (r.is_infinity ());
}
#endif

TEST (Rational_test, init_float_zero)
{
  const Rational r (0.0);
  EQUAL (0, r.sign ());
  EQUAL (0, r.num ());
  EQUAL (1, r.den ());
}

#if 0 // TODO: debug initialization by infinities
TEST (Rational_test, init_float_pos_inf)
{
  const Rational r (INFINITY);
  EQUAL (1, r.sign ());
  CHECK (r.is_infinity ());
  CHECK (std::isinf (static_cast<double> (r)));
}

TEST (Rational_test, init_float_neg_inf)
{
  const Rational r (-INFINITY);
  EQUAL (-1, r.sign ());
  CHECK (r.is_infinity ());
  CHECK (std::isinf (static_cast<double> (r)));
}
#endif

TEST (Rational_test, to_int)
{
  for (int i = -6; i <= 6; ++i)
    {
      EQUAL (i / 2, Rational (i, 2).to_int ());
      EQUAL (i / 3, Rational (i, 3).to_int ());
    }

  if (false) // TODO: debug
    {
      // TODO: Program takes a lot of time without the +1.  Investigate.
      const auto L = std::numeric_limits<int64_t>::lowest () + 1;
      for (int i = 1; i <= 3; ++i)
        {
          EQUAL (L / i, Rational (L, i).to_int ());
        }
    }

  if (false) // TODO: debug
    {
      const auto H = std::numeric_limits<int64_t>::max ();
      for (int i = 1; i <= 3; ++i)
        {
          EQUAL (H / i, Rational (H, i).to_int ());
        }
    }
}
