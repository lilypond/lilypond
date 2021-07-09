/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "interval.hh"

#include "interval.tcc"

#include "yaffut.hh"

#include <cmath>
#include <string>

namespace
{

// This helps see which operations Interval_t<T> requires of T.
//
// This is a sandbox, not a design specification.  If something is omitted or
// deleted here, it doesn't mean that it is forever forbidden from use.
class Mint // mock int
{
private:
  using M = Mint; // shorthand
  int v_ {};

public:
  Mint () = default;
  Mint (const Mint &) = default;
  Mint (Mint &&) = default;
  ~Mint () = default;

  // conversion
  constexpr explicit Mint (int v) : v_ (v) {}

  // assignment
  M &operator = (const M &) = default;
  M &operator = (M &&) = default;

  // negation
  friend constexpr M operator - (M a) { return M (-a.v_); }

  // addition
  M &operator += (M b) { v_ += b.v_; return *this; }
  M &operator -= (M b) { v_ -= b.v_; return *this; }
  friend constexpr M operator + (M a, M b) { return M (a.v_ + b.v_); }
  friend constexpr M operator - (M a, M b) { return M (a.v_ - b.v_); }

  // scaling
  M &operator *= (int) = delete;
  M &operator /= (int) = delete;
  friend constexpr M operator * (M, int) = delete;
  friend constexpr M operator / (M a, int b) { return M (a.v_ / b); }

  // comparison
  friend constexpr bool operator != (M a, M b) { return a.v_ != b.v_; }
  friend constexpr bool operator < (M a, M b) { return a.v_ < b.v_; }
  friend constexpr bool operator <= (M a, M b) { return a.v_ <= b.v_; }
  friend constexpr bool operator == (M a, M b) { return a.v_ == b.v_; }
  friend constexpr bool operator > (M a, M b) { return a.v_ > b.v_; }
  friend constexpr bool operator >= (M a, M b) { return a.v_ >= b.v_; }

  friend std::string to_string (M a) { return std::to_string (a.v_); }
};

}

template<>
Mint
Interval_t<Mint>::infinity ()
{
  return Mint (100);
}

static inline std::ostream &
operator << (std::ostream &os, Mint m) // for Yaffut
{
  return os << to_string (m);
}

template INTERVAL__INSTANTIATE (Mint);

using IVM = Interval_t<Mint>;

class Interval_test
{
#if 0 // TODO: constexpr construction needs constexpr infinity ()
  static void test_init_default ()
  {
    constexpr IVM iv;

    static_assert (iv.left () == Mint (100), "");
    static_assert (iv.right () == Mint (-100), "");

    static_assert (iv.is_empty (), "");
  }
#endif

  static void test_init_point ()
  {
    constexpr IVM iv (Mint (23));

    static_assert (iv.left () == Mint (23), "");
    static_assert (iv.right () == Mint (23), "");

    static_assert (!iv.is_empty (), "");
  }

  static void test_init_list ()
  {
    constexpr IVM iv {Mint (10), Mint (20)};

    static_assert (iv.left () == Mint (10), "");
    static_assert (iv.right () == Mint (20), "");

    static_assert (!iv.is_empty (), "");
  }

  static void test_init_list_assign ()
  {
    constexpr IVM iv = {Mint (40), Mint (30)};

    static_assert (iv.left () == Mint (40), "");
    static_assert (iv.right () == Mint (30), "");

    static_assert (iv.is_empty (), "");
  }

  static Interval_t<int> test_init_implicit_conversion ()
  {
    constexpr Interval_t<signed char> input_iv {-2, 3};

    // Interval_t<signed char> can be converted (here explicitly, below
    // implicitly) to Interval_t<int> because signed char is implicitly
    // convertible to int.
    //
    // TODO: It would be good to verify a negative case too, e.g. that
    // Interval_t<signed char> is not convertible to IVM because
    // signed char is not implicitly convertible to Mint (because the
    // implementation of Mint prevents it).
    constexpr Interval_t<int> test_iv (input_iv);
    static_assert (test_iv.left () == -2, "");
    static_assert (test_iv.right () == 3, "");

    return input_iv; // implicitly converted
  }
};

TEST (Interval_test, center)
{
  // TODO: center() asserts that the interval is not empty.  Maybe it should
  // instead return a specified fallback value so we could test it.

  {
    const IVM iv (Mint (13));
    EQUAL (iv.center (), Mint (13));
  }

  {
    const IVM iv (Mint (10), Mint (20));
    EQUAL (iv.center (), Mint (15));
  }
}

// contains

// TODO: constexpr construction needs constexpr infinity ()
// static_assert (IVM ().contains (Mint (0)) == false, "");
TEST (Interval_test, contains_empty)
{
  const IVM iv;
  CHECK (!iv.contains (Mint (0)));
}

TEST (Interval_test, contains_nonempty)
{
  const IVM iv {Mint (-22), Mint (7)};
  CHECK (!iv.contains (Mint (-23)));
  CHECK (iv.contains (Mint (-22)));
  CHECK (iv.contains (Mint (0)));
  CHECK (iv.contains (Mint (4)));
  CHECK (iv.contains (Mint (7)));
  CHECK (!iv.contains (Mint (8)));
}

template <typename T>
class Interval_math_test
{
protected:
  using IVT = Interval_t<T>;

  static T neg_infinity () { return -IVT::infinity (); }
  static T pos_infinity () { return IVT::infinity (); }

protected:
  void test_intersect ()
  {
    // empty v. full
    {
      IVT iv; // empty
      iv.intersect (IVT::longest ());
      // as empty as empty can be
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), neg_infinity ());
    }

    // empty v. nonempty, nonfull
    {
      IVT iv; // empty
      iv.intersect ({T (12), T (34)});
      // as empty as empty can be
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), neg_infinity ());
    }

    // full v. empty
    {
      IVT iv = IVT::longest ();
      iv.intersect (IVT ());
      // as empty as empty can be
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), neg_infinity ());
    }

    // full v. nonempty, nonfull
    {
      IVT iv = IVT::longest ();
      iv.intersect ({T (12), T (34)});
      EQUAL (iv.left (), T (12));
      EQUAL (iv.right (), T (34));
    }

    // nonempty, nonfull
    {
      IVT iv {T (10), T (20)};
      iv.intersect ({T (15), T (50)});
      EQUAL (iv.left (), T (15));
      EQUAL (iv.right (), T (20));
    }

    // reverse, empty -- doesn't need to be specified, but it's nice for
    // robustness that an empty interval remains empty
    {
      IVT iv {T (5), T (-5)};
      iv.intersect ({});
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), neg_infinity ());
    }
  }

  void test_unite ()
  {
    // empty v. full
    {
      IVT iv; // empty
      iv.unite (IVT::longest ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // empty v. nonempty, nonfull
    {
      IVT iv; // empty
      iv.unite ({T (12), T (34)});
      EQUAL (iv.left (), T (12));
      EQUAL (iv.right (), T (34));
    }

    // full v. empty
    {
      IVT iv = IVT::longest ();
      iv.unite (IVT ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // full v. nonempty, nonfull
    {
      IVT iv = IVT::longest ();
      iv.unite ({T (12), T (34)});
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // nonempty, nonfull
    {
      IVT iv {T (10), T (20)};
      iv.unite ({T (15), T (50)});
      EQUAL (iv.left (), T (10));
      EQUAL (iv.right (), T (50));
    }

    // reverse, full -- doesn't need to be specified, but it's nice for
    // robustness that a full interval remains full
    {
      IVT iv {T (5), T (-5)};
      iv.unite (IVT::longest ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }
  }
};

TEST (Interval_math_test<Mint>, intersect_mint)
{
  test_intersect ();
}

TEST (Interval_math_test<double>, intersect_double)
{
  test_intersect ();
}

TEST (Interval_math_test<Mint>, unite_mint)
{
  test_unite ();
}

TEST (Interval_math_test<double>, unite_double)
{
  test_unite ();
}

static_assert (Interval {-INFINITY, -INFINITY}.is_empty () == false, "");
static_assert (Interval {-INFINITY, 0}.is_empty () == false, "");
static_assert (Interval {-INFINITY, INFINITY}.is_empty () == false, "");
static_assert (Interval {0, -INFINITY}.is_empty () == true, "");
static_assert (Interval {0, INFINITY}.is_empty () == false, "");
static_assert (Interval {INFINITY, -INFINITY}.is_empty () == true, "");
static_assert (Interval {INFINITY, 0}.is_empty () == true, "");
static_assert (Interval {INFINITY, INFINITY}.is_empty () == false, "");

TEST (Interval_test, is_empty_double_nan)
{
  // We can't test with static_assert because GCC does not handle NaN
  // comparisons consistently.  0 > NaN is constexpr, but NaN > 0 isn't.
  // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=88173

  {
    constexpr Interval iv {-NAN, -NAN};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {-NAN, 0};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {-NAN, NAN};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {0, -NAN};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {0, NAN};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {NAN, -NAN};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {NAN, 0};
    CHECK (!iv.is_empty ());
  }

  {
    constexpr Interval iv {NAN, NAN};
    CHECK (!iv.is_empty ());
  }
}

TEST (Interval_test, length)
{
  {
    const IVM iv;
    EQUAL (iv.length (), Mint (0));
  }

  {
    const IVM iv (Mint (4));
    EQUAL (iv.length (), Mint (0));
  }

  {
    const IVM iv {Mint (5), Mint (8)};
    EQUAL (iv.length (), Mint (3));
  }

  {
    const IVM iv {Mint (1), Mint (-1)};
    EQUAL (iv.length (), Mint (0));
  }
}

TEST (Interval_test, length_double_infinity)
{
  if (false) // TODO: (-infinity) - (-infinity) is undefined
    {
      Interval iv {-INFINITY, -INFINITY};
      CHECK (std::isnan (iv.length ()));
    }

  {
    Interval iv {-INFINITY, 0};
    EQUAL (iv.length (), INFINITY);
  }

  {
    Interval iv {-INFINITY, INFINITY};
    EQUAL (iv.length (), INFINITY);
  }

  {
    Interval iv {0, -INFINITY};
    EQUAL (iv.length (), 0);
  }

  {
    Interval iv {0, INFINITY};
    EQUAL (iv.length (), INFINITY);
  }

  {
    Interval iv {INFINITY, -INFINITY};
    EQUAL (iv.length (), 0);
  }

  {
    Interval iv {INFINITY, 0};
    EQUAL (iv.length (), 0);
  }

  if (false) // TODO: infinity - infinity is undefined
    {
      Interval iv {INFINITY, INFINITY};
      CHECK (std::isnan (iv.length ()));
    }
}

TEST (Interval_test, length_double_nan)
{
  {
    Interval iv {0, NAN};
    CHECK (std::isnan (iv.length ()));
  }

  {
    Interval iv {NAN, 0};
    CHECK (std::isnan (iv.length ()));
  }

  {
    Interval iv {-NAN, NAN};
    CHECK (std::isnan (iv.length ()));
  }

  {
    Interval iv {NAN, -NAN};
    CHECK (std::isnan (iv.length ()));
  }
}

TEST (Interval_test, longest)
{
  const auto iv = IVM::longest ();
  EQUAL (iv.left (), Mint (-100));
  EQUAL (iv.right (), Mint (100));
}

TEST (Interval_test, set_empty)
{
  IVM iv {Mint (-33), Mint (33)};
  iv.set_empty ();
  EQUAL (iv.left (), Mint (100));
  EQUAL (iv.right (), Mint (-100));
}

TEST (Interval_test, set_full)
{
  IVM iv {Mint (-33), Mint (33)};
  iv.set_full ();
  EQUAL (iv.left (), Mint (-100));
  EQUAL (iv.right (), Mint (100));
}

TEST (Interval_test, unite)
{
  const IVM empty_iv;
  const auto full_iv = IVM::longest ();

  {
    auto iv = empty_iv;
    iv.unite (empty_iv);
    CHECK (iv.is_empty ());
  }

  {
    auto iv = empty_iv;
    iv.unite (full_iv);
    CHECK (!iv.is_empty ());
  }

  {
    auto iv = full_iv;
    iv.unite (empty_iv);
    CHECK (!iv.is_empty ());
  }

  {
    auto iv = full_iv;
    iv.unite (full_iv);
    CHECK (!iv.is_empty ());
  }
}

TEST (Interval_test, convert_to_string)
{
  {
    IVM iv {Mint (17), Mint (-1)};
    EQUAL (to_string (iv), "[empty]");
  }

  {
    IVM iv {Mint (-24), Mint (68)};
    EQUAL (to_string (iv), "[-24,68]");
  }
}
