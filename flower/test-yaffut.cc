/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Daniel Eble <dan@faithful.be>

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

#include "yaffut.hh"

#include <cassert>
#include <cmath>
#include <functional>

#ifdef NDEBUG
#warning Yaffut self-tests signal failure with assert ().
#endif

namespace
{
class Yaffut_self_test
{
protected:
  void expect_pass (std::function<void (void)> body)
  {
    try
      {
        body ();
      }
    catch (...)
      {
        const bool caught_exception = true;
        assert (!caught_exception);
      }
  }

  void expect_fail (std::function<void (void)> body)
  {
    try
      {
        body ();
        const bool caught_exception = false;
        assert (caught_exception);
      }
    catch (yaffut::failure &)
      {
        // pass
      }
    catch (...)
      {
        const bool caught_unexpected_exception = true;
        assert (!caught_unexpected_exception);
      }
  }
};
}

// NaN equals nothing, not even itself.
TEST (Yaffut_self_test, equal_double_nan_v_float_nan)
{
  const double nan_d = NAN;
  const float nan_f = NAN;
  expect_fail ([&] { yaffut::equal (nan_f, nan_d); });
}

// NaN equals nothing, not even itself.
TEST (Yaffut_self_test, unequal_double_nan_v_float_nan)
{
  const double nan_d = NAN;
  const float nan_f = NAN;
  expect_pass ([&] { yaffut::unequal (nan_f, nan_d); });
}

// NaN equals nothing, not even itself.
TEST (Yaffut_self_test, equal_double_nan)
{
  const double nan = NAN;
  expect_fail ([&] { yaffut::equal (nan, nan); });
}

// NaN equals nothing, not even itself.
TEST (Yaffut_self_test, unequal_double_nan)
{
  const double nan = NAN;
  expect_pass ([&] { yaffut::unequal (nan, nan); });
}

TEST (Yaffut_self_test, equal_double_nan_v_zero)
{
  const double nan = NAN;
  const double zero = 0;
  expect_fail ([&] { yaffut::equal (nan, zero); });
}

TEST (Yaffut_self_test, unequal_double_nan_v_zero)
{
  const double nan = NAN;
  const double zero = 0;
  expect_pass ([&] { yaffut::unequal (nan, zero); });
}

TEST (Yaffut_self_test, equal_double_pos_inf)
{
  const double pos_inf = INFINITY;
  expect_pass ([&] { yaffut::equal (pos_inf, pos_inf); });
}

TEST (Yaffut_self_test, unequal_double_pos_inf)
{
  const double pos_inf = INFINITY;
  expect_fail ([&] { yaffut::unequal (pos_inf, pos_inf); });
}

TEST (Yaffut_self_test, equal_double_pos_inf_v_zero)
{
  const double pos_inf = INFINITY;
  const double zero = 0;
  expect_fail ([&] { yaffut::equal (pos_inf, zero); });
}

TEST (Yaffut_self_test, unequal_double_pos_inf_v_zero)
{
  const double pos_inf = INFINITY;
  const double zero = 0;
  expect_pass ([&] { yaffut::unequal (pos_inf, zero); });
}
