/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include <algorithm>
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

  void expect_fail (std::function<void (void)> body,
                    const char *expected_message_c_str = nullptr)
  {
    try
      {
        body ();
        const bool caught_exception = false;
        assert (caught_exception);
      }
    catch (const yaffut::failure &exception)
      {
        if (expected_message_c_str)
          {
            const std::string exp (expected_message_c_str);
            const std::string act (exception.what ());
            // ignore file & line number: compare just the tail
            if ((exp.size () > act.size ())
                || !std::equal (exp.rbegin (), exp.rend (), act.rbegin ()))
              {
                std::cout << "EXPECTED EXCEPTION MESSAGE:\n"
                          << exp << "\nACTUAL EXCEPTION MESSAGE:\n"
                          << act << "\n";
                assert (false);
              }
          }
      }
    catch (...)
      {
        const bool caught_unexpected_exception = true;
        assert (!caught_unexpected_exception);
      }
  }
};
} // namespace

TEST (Yaffut_self_test, failure_message_assert_throw)
{
  auto do_nothing = [] {};
  expect_fail ([&] { YAFFUT_ASSERT_THROW (do_nothing (), int); },
               ": do_nothing () failed to throw");
}

TEST (Yaffut_self_test, failure_message_check)
{
  const bool phooey = false;
  expect_fail ([&] { YAFFUT_CHECK (phooey); }, ": CHECK(phooey) failed ");
}

TEST (Yaffut_self_test, failure_message_fail)
{
  expect_fail ([&] { YAFFUT_FAIL ("mumble"); }, ": mumble");
}

TEST (Yaffut_self_test, failure_message_equal)
{
  const int a = 123;
  const int b = 456;
  expect_fail ([&] { YAFFUT_EQUAL (a, b); }, ": EQUAL(a == b) failed \n"
                                             "left:  (int) 123\n"
                                             "right: (int) 456");
}

TEST (Yaffut_self_test, failure_message_unequal)
{
  const float f = 1.5;
  const double d = 1.5;
  expect_fail ([&] { YAFFUT_UNEQUAL (f, d); }, ": UNEQUAL(f != d) failed \n"
                                               "left:  (float) 1.5\n"
                                               "right: (double) 1.5");
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
