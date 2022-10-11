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

#include "drul-array.hh"

#include "yaffut.hh"

class Drul_array_test
{
public:
  static constexpr void test_access_const_unchecked ()
  {
    const Drul_array<int> arr {12, 34};
    EQUAL (12, arr.front ());
    EQUAL (34, arr.back ());
  }

  static constexpr void test_access_mutable_unchecked ()
  {
    Drul_array<int> arr {12, 34};

    ++arr.front ();

    EQUAL (13, arr.front ());
    EQUAL (34, arr.back ());

    --arr.back ();

    EQUAL (13, arr.front ());
    EQUAL (33, arr.back ());
  }

  static void test_init_default_int ()
  {
    constexpr Drul_array<int> arr;
    static_assert (arr.front () == 0, "");
    static_assert (arr.back () == 0, "");
  }

  static void test_init_default_dir ()
  {
    constexpr Drul_array<Direction> arr;
    static_assert (arr.front () == CENTER, "");
    static_assert (arr.back () == CENTER, "");
  }

  static void test_init_default_class ()
  {
    struct AdHoc
    {
      int val = 42;
    };
    constexpr Drul_array<AdHoc> arr;
    static_assert (arr.front ().val == 42, "");
    static_assert (arr.back ().val == 42, "");
  }

  static void test_init_value_int ()
  {
    constexpr Drul_array<int> arr {};
    static_assert (arr.front () == 0, "");
    static_assert (arr.back () == 0, "");
  }

  static void test_init_value_dir ()
  {
    constexpr Drul_array<Direction> arr {};
    static_assert (arr.front () == CENTER, "");
    static_assert (arr.back () == CENTER, "");
  }

  static void test_init_value_class ()
  {
    struct AdHoc
    {
      int val = 42;
    };
    constexpr Drul_array<AdHoc> arr {};
    static_assert (arr.front ().val == 42, "");
    static_assert (arr.back ().val == 42, "");
  }

  static void test_init_list ()
  {
    constexpr Drul_array<int> arr {12, 34};
    static_assert (arr.front () == 12, "");
    static_assert (arr.back () == 34, "");
  }

  static void test_init_list_assign ()
  {
    constexpr Drul_array<int> arr = {12, 34};
    static_assert (arr.front () == 12, "");
    static_assert (arr.back () == 34, "");
  }

  static constexpr void test_scaling ()
  {
    Drul_array<int> arr {12, 34};

    scale_drul (&arr, 2);

    EQUAL (24, arr.front ());
    EQUAL (68, arr.back ());
  }
};

TEST (Drul_array_test, access_const_checked)
{
  const Drul_array<int> arr {12, 34};

  EQUAL (12, arr.at (LEFT));
  EQUAL (34, arr.at (RIGHT));

  EQUAL (12, arr[LEFT]);
  EQUAL (34, arr[RIGHT]);
}

static_assert ((Drul_array_test::test_access_const_unchecked (), true), "");

TEST (Drul_array_test, access_mutable_checked)
{
  Drul_array<int> arr {12, 34};

  --arr[LEFT];

  EQUAL (11, arr[LEFT]);
  EQUAL (34, arr[RIGHT]);

  ++arr.at (RIGHT);

  EQUAL (11, arr.at (LEFT));
  EQUAL (35, arr.at (RIGHT));
}

static_assert ((Drul_array_test::test_access_mutable_unchecked (), true), "");

static_assert ((Drul_array_test::test_scaling (), true), "");
