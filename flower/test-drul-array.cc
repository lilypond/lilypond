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

#include "drul-array.hh"

#include "yaffut.hh"

class Drul_array_test
{
  static void test_init_default_int ()
  {
    constexpr Drul_array<int> arr;
    static_assert (arr[LEFT] == 0, "");
    static_assert (arr[RIGHT] == 0, "");
  }

  static void test_init_default_dir ()
  {
    constexpr Drul_array<Direction> arr;
    static_assert (arr[LEFT] == CENTER, "");
    static_assert (arr[RIGHT] == CENTER, "");
  }

  static void test_init_default_class ()
  {
    struct AdHoc { int val = 42; };
    constexpr Drul_array<AdHoc> arr;
    static_assert (arr[LEFT].val == 42, "");
    static_assert (arr[RIGHT].val == 42, "");
  }

  static void test_init_value_int ()
  {
    constexpr Drul_array<int> arr {};
    static_assert (arr[LEFT] == 0, "");
    static_assert (arr[RIGHT] == 0, "");
  }

  static void test_init_value_dir ()
  {
    constexpr Drul_array<Direction> arr {};
    static_assert (arr[LEFT] == CENTER, "");
    static_assert (arr[RIGHT] == CENTER, "");
  }

  static void test_init_value_class ()
  {
    struct AdHoc { int val = 42; };
    constexpr Drul_array<AdHoc> arr {};
    static_assert (arr[LEFT].val == 42, "");
    static_assert (arr[RIGHT].val == 42, "");
  }

  static void test_init_list ()
  {
    constexpr Drul_array<int> arr {12, 34};
    static_assert (arr.at (LEFT) == 12, "");
    static_assert (arr.at (RIGHT) == 34, "");
  }

  static void test_init_list_assign ()
  {
    constexpr Drul_array<int> arr = {12, 34};
    static_assert (arr[LEFT] == 12, "");
    static_assert (arr[RIGHT] == 34, "");
  }

  static void test_average ()
  {
    static_assert (Drul_array<int> {10, 15}.average () == 12, "");
    static_assert (Drul_array<Real> {10, 11}.average () == 10.5, "");
  }

  static void test_delta ()
  {
    constexpr Drul_array<int> arr {5, 8};
    static_assert (arr.delta () == (8 - 5), "");
  }
};

TEST (Drul_array_test, mutable_access)
{
  Drul_array<int> arr {12, 34};

  --arr[LEFT];

  EQUAL (11, arr[LEFT]);
  EQUAL (34, arr[RIGHT]);

  ++arr.at (RIGHT);

  EQUAL (11, arr[LEFT]);
  EQUAL (35, arr[RIGHT]);
}

TEST (Drul_array_test, scaling)
{
  Drul_array<int> arr {12, 34};

  scale_drul (&arr, 2);

  EQUAL (24, arr[LEFT]);
  EQUAL (68, arr[RIGHT]);
}
