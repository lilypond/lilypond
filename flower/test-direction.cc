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

#include "direction.hh"

#include "yaffut.hh"

#include <cmath>

// shorthand for tests below
static constexpr auto NEGA = Direction::negative ();
static constexpr auto ZERO = Direction::zero ();
static constexpr auto POSI = Direction::positive ();

// initialization

static_assert (Direction () == ZERO, "");

static_assert (Direction (-2) == NEGA, "");
static_assert (Direction (-2L) == NEGA, "");
static_assert (Direction (-2LL) == NEGA, "");
static_assert (Direction (0) == ZERO, "");
static_assert (Direction (0L) == ZERO, "");
static_assert (Direction (0LL) == ZERO, "");
static_assert (Direction (2) == POSI, "");
static_assert (Direction (2L) == POSI, "");
static_assert (Direction (2LL) == POSI, "");

// conversion to bool

static_assert (NEGA, "");
static_assert (!ZERO, "");
static_assert (POSI, "");

// conversion to index

static_assert (NEGA.to_index () == 0, "");
static_assert (ZERO.to_index () == 1, "");
static_assert (POSI.to_index () == 2, "");

// unary +/-

static_assert (-NEGA == POSI, "");
static_assert (-ZERO == ZERO, "");
static_assert (-POSI == NEGA, "");

static_assert (+NEGA == NEGA, "");
static_assert (+ZERO == ZERO, "");
static_assert (+POSI == POSI, "");

static_assert (directed_opposite (+POSI, -POSI),
               "unary +/- should yield a Direction");

// multiplication by another Direction

static_assert (NEGA * NEGA == POSI, "");
static_assert (NEGA * ZERO == ZERO, "");
static_assert (NEGA * POSI == NEGA, "");
static_assert (ZERO * NEGA == ZERO, "");
static_assert (ZERO * ZERO == ZERO, "");
static_assert (ZERO * POSI == ZERO, "");
static_assert (POSI * NEGA == NEGA, "");
static_assert (POSI * ZERO == ZERO, "");
static_assert (POSI * POSI == POSI, "");

static_assert (directed_same (POSI * POSI, POSI),
               "(Direction * Direction) should yield a Direction");

static_assert ((Direction (NEGA) *= NEGA) == POSI, "");
static_assert ((Direction (NEGA) *= ZERO) == ZERO, "");
static_assert ((Direction (NEGA) *= POSI) == NEGA, "");
static_assert ((Direction (ZERO) *= NEGA) == ZERO, "");
static_assert ((Direction (ZERO) *= ZERO) == ZERO, "");
static_assert ((Direction (ZERO) *= POSI) == ZERO, "");
static_assert ((Direction (POSI) *= NEGA) == NEGA, "");
static_assert ((Direction (POSI) *= ZERO) == ZERO, "");
static_assert ((Direction (POSI) *= POSI) == POSI, "");

static_assert (directed_same ((Direction (POSI) *= POSI), POSI),
               "(Direction *= Direction) should yield a Direction");

// multiplication by an int

static_assert (-3 * NEGA == 3, "");
static_assert (-3 * ZERO == 0, "");
static_assert (-3 * POSI == -3, "");
static_assert (0 * NEGA == 0, "");
static_assert (0 * ZERO == 0, "");
static_assert (0 * POSI == 0, "");
static_assert (3 * NEGA == -3, "");
static_assert (3 * ZERO == 0, "");
static_assert (3 * POSI == 3, "");

static_assert (NEGA * -3 == 3, "");
static_assert (ZERO * -3 == 0, "");
static_assert (POSI * -3 == -3, "");
static_assert (NEGA * 0 == 0, "");
static_assert (ZERO * 0 == 0, "");
static_assert (POSI * 0 == 0, "");
static_assert (NEGA * 3 == -3, "");
static_assert (ZERO * 3 == 0, "");
static_assert (POSI * 3 == 3, "");

// multiplication by a float
// (but we're not covering special values like signed zero)

static_assert (-4.4 * NEGA == 4.4, "");
static_assert (-4.4 * ZERO == 0, "");
static_assert (-4.4 * POSI == -4.4, "");
static_assert (0.0 * NEGA == 0, "");
static_assert (0.0 * ZERO == 0, "");
static_assert (0.0 * POSI == 0, "");
static_assert (4.4 * NEGA == -4.4, "");
static_assert (4.4 * ZERO == 0, "");
static_assert (4.4 * POSI == 4.4, "");

static_assert (NEGA * -4.4 == 4.4, "");
static_assert (ZERO * -4.4 == 0, "");
static_assert (POSI * -4.4 == -4.4, "");
static_assert (NEGA * 0.0 == 0, "");
static_assert (ZERO * 0.0 == 0, "");
static_assert (POSI * 0.0 == 0, "");
static_assert (NEGA * 4.4 == -4.4, "");
static_assert (ZERO * 4.4 == 0, "");
static_assert (POSI * 4.4 == 4.4, "");

// comparison

// (cutting corners for the operators because they are compiler-generated)
static_assert ((NEGA < ZERO) == true, "");
static_assert ((ZERO < POSI) == true, "");
static_assert ((POSI < ZERO) == false, "");
static_assert ((ZERO < NEGA) == false, "");

static_assert (directed_same (NEGA, NEGA) == true, "");
static_assert (directed_same (NEGA, ZERO) == false, "");
static_assert (directed_same (NEGA, POSI) == false, "");
static_assert (directed_same (ZERO, NEGA) == false, "");
static_assert (directed_same (ZERO, ZERO) == false, "");
static_assert (directed_same (ZERO, POSI) == false, "");
static_assert (directed_same (POSI, NEGA) == false, "");
static_assert (directed_same (POSI, ZERO) == false, "");
static_assert (directed_same (POSI, POSI) == true, "");

static_assert (directed_opposite (NEGA, NEGA) == false, "");
static_assert (directed_opposite (NEGA, ZERO) == false, "");
static_assert (directed_opposite (NEGA, POSI) == true, "");
static_assert (directed_opposite (ZERO, NEGA) == false, "");
static_assert (directed_opposite (ZERO, ZERO) == false, "");
static_assert (directed_opposite (ZERO, POSI) == false, "");
static_assert (directed_opposite (POSI, NEGA) == true, "");
static_assert (directed_opposite (POSI, ZERO) == false, "");
static_assert (directed_opposite (POSI, POSI) == false, "");

static_assert (minmax (NEGA, 1, 2) == 1, "");
static_assert (minmax (NEGA, 2, 1) == 1, "");
static_assert (minmax (POSI, 1, 2) == 2, "");
static_assert (minmax (POSI, 2, 1) == 2, "");

class Direction_test
{
public:
  static constexpr void test_copy_construct ()
  {
    const Direction D (-6);
    const Direction d (D);
    EQUAL (d, NEGA);
  }

  static constexpr void test_copy_assign ()
  {
    const Direction D (7);
    Direction d;
    d = D;
    EQUAL (d, POSI);
  }

  static int test_switch (Direction d)
  {
    switch (d) // the test is that this compiles
      {
      case NEGA:
        return -1;
      case ZERO:
        return 0;
      case POSI:
        return 1;
      }

    // Alas, it would be nice to disallow conversion to int in this scenario
    // because an impossible case shows the author wasn't thinking clearly.
    switch (d)
      {
      case -666: // way, way DOWN
        return false;
      default:
        return true;
      }
  }
};

static_assert ((Direction_test::test_copy_construct (), true), "");

static_assert ((Direction_test::test_copy_assign (), true), "");

TEST (Direction_test, init_float)
{
  EQUAL (Direction (-5.5), NEGA);
  EQUAL (Direction (-INFINITY), NEGA);
  EQUAL (Direction (-NAN), NEGA);

  EQUAL (Direction (-0.0), ZERO);
  EQUAL (Direction (0.0), ZERO);

  EQUAL (Direction (5.5), POSI);
  EQUAL (Direction (INFINITY), POSI);
  EQUAL (Direction (NAN), POSI);
}
