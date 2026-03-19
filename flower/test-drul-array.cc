/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2026 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include <type_traits>

namespace
{

template <typename T>
constexpr bool
is_lvalue_ref (T &&)
{
  return std::is_lvalue_reference<T> ();
}

} // namespace

class Drul_array_test
{
public:
  static constexpr void test_access_checked_const ()
  {
    const Drul_array<int> arr {12, 34};

    EQUAL (12, arr.at (LEFT));
    EQUAL (34, arr.at (RIGHT));

    EQUAL (12, arr[LEFT]);
    EQUAL (34, arr[RIGHT]);
  }

  static constexpr void test_access_checked_const_temp ()
  {
    using CD = const Drul_array<int>;
    EQUAL (12, (CD {12, 34}).at (LEFT));
    EQUAL (34, (CD {12, 34}).at (RIGHT));
    EQUAL (12, (CD {12, 34})[LEFT]);
    EQUAL (34, (CD {12, 34})[RIGHT]);

    // no backdoor to create an lvalue reference to a temporary
    CHECK (!is_lvalue_ref (CD {}.at (LEFT)));
    CHECK (!is_lvalue_ref (CD {}.at (RIGHT)));
    CHECK (!is_lvalue_ref (CD {}[LEFT]));
    CHECK (!is_lvalue_ref (CD {}[RIGHT]));
  }

  static constexpr void test_access_checked_mutable ()
  {
    Drul_array<int> arr {12, 34};

    --arr[LEFT];

    EQUAL (11, arr[LEFT]);
    EQUAL (34, arr[RIGHT]);

    ++arr.at (RIGHT);

    EQUAL (11, arr.at (LEFT));
    EQUAL (35, arr.at (RIGHT));
  }

  static constexpr void test_access_checked_mutable_temp ()
  {
    EQUAL (12, (Drul_array<int> {12, 34})[LEFT]);
    EQUAL (34, (Drul_array<int> {12, 34})[RIGHT]);
    EQUAL (12, (Drul_array<int> {12, 34}).at (LEFT));
    EQUAL (34, (Drul_array<int> {12, 34}).at (RIGHT));

    // no backdoor to create an lvalue reference to a temporary
    CHECK (!is_lvalue_ref (Drul_array<int> {}[LEFT]));
    CHECK (!is_lvalue_ref (Drul_array<int> {}[RIGHT]));
    CHECK (!is_lvalue_ref (Drul_array<int> {}.at (LEFT)));
    CHECK (!is_lvalue_ref (Drul_array<int> {}.at (RIGHT)));
  }

  static constexpr void test_access_unchecked_const ()
  {
    const Drul_array<int> arr {12, 34};
    EQUAL (12, arr.front ());
    EQUAL (34, arr.back ());
    EQUAL (12, get<0> (arr));
    EQUAL (34, get<1> (arr));
  }

  static constexpr void test_access_unchecked_const_temp ()
  {
    using CD = const Drul_array<int>;
    EQUAL (12, (CD {12, 34}).front ());
    EQUAL (34, (CD {12, 34}).back ());
    EQUAL (12, get<0> (CD {12, 34}));
    EQUAL (34, get<1> (CD {12, 34}));

    // no backdoor to create an lvalue reference to a temporary
    CHECK (!is_lvalue_ref (CD {}.front ()));
    CHECK (!is_lvalue_ref (CD {}.back ()));
    CHECK (!is_lvalue_ref (get<0> (CD {})));
    CHECK (!is_lvalue_ref (get<1> (CD {})));
  }

  static constexpr void test_access_unchecked_mutable ()
  {
    Drul_array<int> arr {12, 34};

    ++arr.front ();

    EQUAL (13, arr.front ());
    EQUAL (34, arr.back ());
    EQUAL (13, get<0> (arr));
    EQUAL (34, get<1> (arr));

    --arr.back ();

    EQUAL (13, arr.front ());
    EQUAL (33, arr.back ());
    EQUAL (13, get<0> (arr));
    EQUAL (33, get<1> (arr));
  }

  static constexpr void test_access_unchecked_mutable_temp ()
  {
    EQUAL (12, (Drul_array<int> {12, 34}).front ());
    EQUAL (34, (Drul_array<int> {12, 34}).back ());
    EQUAL (12, get<0> (Drul_array<int> {12, 34}));
    EQUAL (34, get<1> (Drul_array<int> {12, 34}));

    // no backdoor to create an lvalue reference to a temporary
    CHECK (!is_lvalue_ref (Drul_array<int> {}.front ()));
    CHECK (!is_lvalue_ref (Drul_array<int> {}.back ()));
    CHECK (!is_lvalue_ref (get<0> (Drul_array<int> {})));
    CHECK (!is_lvalue_ref (get<1> (Drul_array<int> {})));
  }

  static void test_init_default_int ()
  {
    constexpr Drul_array<int> arr;
    static_assert (arr.front () == 0);
    static_assert (arr.back () == 0);
  }

  static void test_init_default_dir ()
  {
    constexpr Drul_array<Direction> arr;
    static_assert (arr.front () == CENTER);
    static_assert (arr.back () == CENTER);
  }

  static void test_init_default_class ()
  {
    struct AdHoc
    {
      int val = 42;
    };
    constexpr Drul_array<AdHoc> arr;
    static_assert (arr.front ().val == 42);
    static_assert (arr.back ().val == 42);
  }

  static void test_init_value_int ()
  {
    constexpr Drul_array<int> arr {};
    static_assert (arr.front () == 0);
    static_assert (arr.back () == 0);
  }

  static void test_init_value_dir ()
  {
    constexpr Drul_array<Direction> arr {};
    static_assert (arr.front () == CENTER);
    static_assert (arr.back () == CENTER);
  }

  static void test_init_value_class ()
  {
    struct AdHoc
    {
      int val = 42;
    };
    constexpr Drul_array<AdHoc> arr {};
    static_assert (arr.front ().val == 42);
    static_assert (arr.back ().val == 42);
  }

  static void test_init_list ()
  {
    constexpr Drul_array<int> arr {12, 34};
    static_assert (arr.front () == 12);
    static_assert (arr.back () == 34);
  }

  static void test_init_list_assign ()
  {
    constexpr Drul_array<int> arr = {12, 34};
    static_assert (arr.front () == 12);
    static_assert (arr.back () == 34);
  }

  static constexpr void test_scaling ()
  {
    Drul_array<int> arr {12, 34};

    scale_drul (&arr, 2);

    EQUAL (24, arr.front ());
    EQUAL (68, arr.back ());
  }

  static constexpr void test_structured_bind_const_lval ()
  {
    constexpr Drul_array<int> arr {12, 34};
    const auto &[left, right] = arr;

    EQUAL (12, left);
    EQUAL (34, right);
  }

  static constexpr void test_structured_bind_const_rval ()
  {
    const auto [left, right] = Drul_array<int> {12, 34};

    EQUAL (12, left);
    EQUAL (34, right);
  }

  static constexpr void test_structured_bind_mutable_lval ()
  {
    Drul_array<int> arr {12, 34};
    auto &[left, right] = arr;

    left = 11;
    right = 22;

    EQUAL (11, arr.front ());
    EQUAL (22, arr.back ());
  }

  static constexpr void test_structured_bind_mutable_rval ()
  {
    auto [left, right] = Drul_array<int> {9, 11};

    left += 1;
    right -= 1;

    EQUAL (left, right);
  }

  static constexpr void test_tuple_size ()
  {
    constexpr auto z = std::tuple_size<Drul_array<int>>::value;

    EQUAL (2U, z);
  }
};

static_assert ((Drul_array_test::test_access_checked_const (), true));
static_assert ((Drul_array_test::test_access_checked_const_temp (), true));
static_assert ((Drul_array_test::test_access_checked_mutable (), true));
static_assert ((Drul_array_test::test_access_checked_mutable_temp (), true));

static_assert ((Drul_array_test::test_access_unchecked_const (), true));
static_assert ((Drul_array_test::test_access_unchecked_const_temp (), true));
static_assert ((Drul_array_test::test_access_unchecked_mutable (), true));
static_assert ((Drul_array_test::test_access_unchecked_mutable_temp (), true));

static_assert ((Drul_array_test::test_scaling (), true));

static_assert ((Drul_array_test::test_structured_bind_const_lval (), true));

static_assert ((Drul_array_test::test_structured_bind_const_rval (), true));

static_assert ((Drul_array_test::test_structured_bind_mutable_lval (), true));

static_assert ((Drul_array_test::test_structured_bind_mutable_rval (), true));

static_assert ((Drul_array_test::test_tuple_size (), true));
