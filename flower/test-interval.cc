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

#include "interval.hh"

#include "interval.tcc"

#include "yaffut.hh"

#include <chrono>
#include <cmath>
#include <limits>
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
  constexpr explicit Mint (int v)
    : v_ (v)
  {
  }

  // special values
  static constexpr M infinity () { return M (100); }

  // assignment
  M &operator= (const M &) = default;
  M &operator= (M &&) = default;

  // negation
  friend constexpr M operator- (M a) { return M (-a.v_); }

  // addition
  M &operator+= (M b)
  {
    v_ += b.v_;
    return *this;
  }
  M &operator-= (M b)
  {
    v_ -= b.v_;
    return *this;
  }
  friend constexpr M operator+ (M a, M b) { return M (a.v_ + b.v_); }
  friend constexpr M operator- (M a, M b) { return M (a.v_ - b.v_); }

  // scaling
  M &operator*= (int) = delete;
  M &operator/= (int) = delete;
  friend constexpr M operator* (M, int) = delete;
  friend constexpr M operator/ (M a, int b) { return M (a.v_ / b); }

  // comparison
  friend constexpr bool operator!= (M a, M b) { return a.v_ != b.v_; }
  friend constexpr bool operator<(M a, M b) { return a.v_ < b.v_; }
  friend constexpr bool operator<= (M a, M b) { return a.v_ <= b.v_; }
  friend constexpr bool operator== (M a, M b) { return a.v_ == b.v_; }
  friend constexpr bool operator> (M a, M b) { return a.v_ > b.v_; }
  friend constexpr bool operator>= (M a, M b) { return a.v_ >= b.v_; }

  friend std::string to_string (M a) { return std::to_string (a.v_); }
};

static inline std::ostream &
operator<< (std::ostream &os, Mint m) // for Yaffut
{
  return os << to_string (m);
}

} // namespace

template INTERVAL__INSTANTIATE (Mint);

using IVM = Interval_t<Mint>;

class Interval_test
{
  static void test_init_default ()
  {
    constexpr IVM iv;

    static_assert (iv.left () == Mint (100), "");
    static_assert (iv.right () == Mint (-100), "");

    static_assert (iv.length () == Mint (0), "");
  }

  static void test_init_point ()
  {
    constexpr IVM iv (Mint (23));

    static_assert (iv.left () == Mint (23), "");
    static_assert (iv.right () == Mint (23), "");

    static_assert (iv.length () == Mint (0), "");
  }

  static void test_init_list ()
  {
    constexpr IVM iv {Mint (10), Mint (20)};

    static_assert (iv.left () == Mint (10), "");
    static_assert (iv.right () == Mint (20), "");

    static_assert (iv.length () == Mint (10), "");
  }

  static void test_init_list_assign ()
  {
    constexpr IVM iv = {Mint (40), Mint (30)};

    static_assert (iv.left () == Mint (40), "");
    static_assert (iv.right () == Mint (30), "");

    static_assert (iv.length () == Mint (0), "");
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

  static void test_longest ()
  {
    constexpr auto iv = IVM::longest ();
    static_assert (iv.left () == Mint (-100), "");
    static_assert (iv.right () == Mint (100), "");
  }

  static void test_std_chrono_time_point ()
  {
    using Clock = std::chrono::steady_clock;
    constexpr Clock::duration d (0);
    constexpr Clock::time_point tp (d);

    Interval_t<Clock::time_point> iv {tp, tp};

    // Checking arithmetic is a job for other test cases.  The goal here is to
    // show that that the types (dimensions) are correct.
    static_cast<void> (iv = (iv += d));
    static_cast<void> (iv = (iv -= d));
    static_cast<void> (iv = intersection (iv, iv));
    static_cast<void> (iv = iv + d);
    static_cast<void> (iv = iv - d);
    static_cast<void> (iv = iv.union_disjoint (iv, d, Direction::positive ()));
    static_cast<void> (iv.center () == tp);
    static_cast<void> (iv.clamp (tp) == tp);
    static_cast<void> (iv.contains (tp));
    static_cast<void> (iv.distance (tp) == d);
    static_cast<void> (iv.intersect (iv));
    static_cast<void> (iv.is_empty ());
    static_cast<void> (iv.left_less (iv, iv));
    static_cast<void> (iv.length () == d);
    static_cast<void> (iv.swap ());
    static_cast<void> (iv.translate (d));
    static_cast<void> (iv.unite (iv));
    static_cast<void> (iv.unite_disjoint (iv, d, Direction::negative ()));
    static_cast<void> (iv.widen (d));
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

static_assert (IVM ().contains (Mint (0)) == false, "");

static_assert (IVM (Mint (4)).contains (Mint (3)) == false, "");
static_assert (IVM (Mint (4)).contains (Mint (4)) == true, "");
static_assert (IVM (Mint (4)).contains (Mint (5)) == false, "");

static_assert (IVM (Mint (-22), Mint (7)).contains (Mint (-23)) == false, "");
static_assert (IVM (Mint (-22), Mint (7)).contains (Mint (-22)) == true, "");
static_assert (IVM (Mint (-22), Mint (7)).contains (Mint (0)) == true, "");
static_assert (IVM (Mint (-22), Mint (7)).contains (Mint (7)) == true, "");
static_assert (IVM (Mint (-22), Mint (7)).contains (Mint (8)) == false, "");

template <typename T>
class Interval_math_test
{
protected:
  using IVT = Interval_t<T>;

  static constexpr T neg_infinity () { return Interval_traits<T>::min (); }
  static constexpr T pos_infinity () { return Interval_traits<T>::max (); }

protected:
  void test_add_point ()
  {
    // empty; add -inf
    {
      IVT iv; // empty
      iv.add_point (neg_infinity ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), neg_infinity ());
    }

    // empty; add 0
    {
      IVT iv; // empty
      iv.add_point (T (0));
      EQUAL (iv.left (), T (0));
      EQUAL (iv.right (), T (0));
      EQUAL (iv.length (), T (0));
    }

    // empty; add +inf
    {
      IVT iv; // empty
      iv.add_point (pos_infinity ());
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // full; add -inf
    {
      IVT iv = IVT::longest ();
      iv.add_point (neg_infinity ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // full; add 0
    {
      IVT iv = IVT::longest ();
      iv.add_point (T (0));
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // full; add +inf
    {
      IVT iv = IVT::longest ();
      iv.add_point (pos_infinity ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
    }

    // nonempty, nonfull; add -inf
    {
      IVT iv {T (10), T (20)};
      iv.add_point (neg_infinity ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), T (20));
    }

    // nonempty, nonfull; add point < left
    {
      IVT iv {T (10), T (20)};
      iv.add_point (T (1));
      EQUAL (iv.left (), T (1));
      EQUAL (iv.right (), T (20));
    }

    // nonempty, nonfull; add point already in interval
    {
      IVT iv {T (10), T (20)};
      iv.add_point (T (0));
      EQUAL (iv.left (), T (0));
      EQUAL (iv.right (), T (20));
    }

    // nonempty, nonfull; add point > right
    {
      IVT iv {T (10), T (20)};
      iv.add_point (T (21));
      EQUAL (iv.left (), T (10));
      EQUAL (iv.right (), T (21));
    }

    // nonempty, nonfull; add +inf
    {
      IVT iv {T (10), T (20)};
      iv.add_point (pos_infinity ());
      EQUAL (iv.left (), T (10));
      EQUAL (iv.right (), pos_infinity ());
    }
  }

  void test_clamp ()
  {
    // empty
    {
      constexpr IVT iv;
      static_assert (iv.clamp (neg_infinity ()) == neg_infinity (), "");
      static_assert (iv.clamp (T (0)) == T (0), "");
      static_assert (iv.clamp (pos_infinity ()) == pos_infinity (), "");
    }

    // nonempty, nonfull
    {
      constexpr IVT iv {T (10), T (20)};
      static_assert (iv.clamp (T (neg_infinity ())) == T (10), "");
      static_assert (iv.clamp (T (9)) == T (10), "");
      static_assert (iv.clamp (T (10)) == T (10), "");
      static_assert (iv.clamp (T (11)) == T (11), "");
      static_assert (iv.clamp (T (19)) == T (19), "");
      static_assert (iv.clamp (T (20)) == T (20), "");
      static_assert (iv.clamp (T (21)) == T (20), "");
      static_assert (iv.clamp (T (pos_infinity ())) == T (20), "");
    }

    // full
    {
      constexpr IVT iv = IVT::longest ();
      static_assert (iv.clamp (neg_infinity ()) == neg_infinity (), "");
      static_assert (iv.clamp (T (0)) == T (0), "");
      static_assert (iv.clamp (pos_infinity ()) == pos_infinity (), "");
    }
  }

  void test_empty ()
  {
    constexpr auto z = T (0);
    constexpr auto p = pos_infinity ();

    static_assert (IVT {}.is_empty () == true, "");

    static_assert (IVT {z}.is_empty () == false, "");
    static_assert (IVT {p}.is_empty () == false, "");

    static_assert (IVT {z, z}.is_empty () == false, "");
    static_assert (IVT {z, p}.is_empty () == false, "");
    static_assert (IVT {p, z}.is_empty () == true, "");
    static_assert (IVT {p, p}.is_empty () == false, "");

    constexpr auto n = neg_infinity ();
    if (z != n) // more cases for signed types
      {
        CHECK (IVT (n).is_empty () == false);

        CHECK (IVT (n, n).is_empty () == false);
        CHECK (IVT (n, z).is_empty () == false);
        CHECK (IVT (n, p).is_empty () == false);
        CHECK (IVT (z, n).is_empty () == true);
        CHECK (IVT (p, n).is_empty () == true);
      }
  }

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

    // empty v. empty
    {
      IVT iv; // empty
      iv.intersect (IVT ());
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

    // full v. full
    {
      IVT iv = IVT::longest ();
      iv.intersect (IVT::longest ());
      EQUAL (iv.left (), neg_infinity ());
      EQUAL (iv.right (), pos_infinity ());
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

    // empty v. empty
    {
      IVT iv; // empty
      iv.unite (IVT ());
      // as empty as empty can be
      EQUAL (iv.left (), pos_infinity ());
      EQUAL (iv.right (), neg_infinity ());
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

    // full v. full
    {
      IVT iv = IVT::longest ();
      iv.unite (IVT::longest ());
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

TEST (Interval_math_test<Mint>, add_point_mint)
{
  test_add_point ();
}

TEST (Interval_math_test<double>, add_point_double)
{
  test_add_point ();
}

TEST (Interval_math_test<vsize>, add_point_vsize)
{
  test_add_point ();
}

TEST (Interval_math_test<Mint>, clamp_mint)
{
  test_clamp ();
}

TEST (Interval_math_test<double>, clamp_double)
{
  test_clamp ();
}

TEST (Interval_math_test<vsize>, clamp_vsize)
{
  test_clamp ();
}

TEST (Interval_math_test<Mint>, empty_mint)
{
  test_empty ();
}

TEST (Interval_math_test<double>, empty_double)
{
  test_empty ();
}

TEST (Interval_math_test<vsize>, empty_vsize)
{
  test_empty ();
}

TEST (Interval_math_test<Mint>, intersect_mint)
{
  test_intersect ();
}

TEST (Interval_math_test<double>, intersect_double)
{
  test_intersect ();
}

TEST (Interval_math_test<vsize>, intersect_vsize)
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

TEST (Interval_math_test<vsize>, unite_vsize)
{
  test_unite ();
}

TEST (Interval_test, clamp_double_nan)
{
  {
    constexpr Interval iv; // empty
    CHECK (std::isnan (iv.clamp (NAN)));
    CHECK (std::isnan (iv.clamp (-NAN)));
  }

  {
    constexpr Interval iv {17};
    CHECK (std::isnan (iv.clamp (NAN)));
    CHECK (std::isnan (iv.clamp (-NAN)));
  }

  {
    constexpr Interval iv {-5, 5};
    CHECK (std::isnan (iv.clamp (NAN)));
    CHECK (std::isnan (iv.clamp (-NAN)));
  }

  {
    constexpr Interval iv {-NAN, 0};
    EQUAL (iv.clamp (-INFINITY), -INFINITY);
    EQUAL (iv.clamp (-1), -1);
    EQUAL (iv.clamp (0), 0);
    EQUAL (iv.clamp (1), 0);
    EQUAL (iv.clamp (INFINITY), 0);
  }

  {
    constexpr Interval iv {0, NAN};
    EQUAL (iv.clamp (-INFINITY), 0);
    EQUAL (iv.clamp (-1), 0);
    EQUAL (iv.clamp (0), 0);
    EQUAL (iv.clamp (1), 1);
    EQUAL (iv.clamp (INFINITY), INFINITY);
  }

  {
    constexpr Interval iv {-NAN, NAN};
    EQUAL (iv.clamp (-INFINITY), -INFINITY);
    EQUAL (iv.clamp (0), 0);
    EQUAL (iv.clamp (INFINITY), INFINITY);
  }
}

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

TEST (Interval_test, length_double_infinity)
{
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

TEST (Interval_test, linear_combination)
{
  // TODO: Cover empty_interval.linear_combination () and other weird cases.

  // point
  {
    const Interval iv {42};
    // TODO: Wouldn't -INFINITY would be a better result than NAN?
    CHECK (std::isnan (iv.linear_combination (-INFINITY)));
    EQUAL (iv.linear_combination (-2), 42);
    EQUAL (iv.linear_combination (-1), 42);
    EQUAL (iv.linear_combination (0), 42);
    EQUAL (iv.linear_combination (1), 42);
    EQUAL (iv.linear_combination (2), 42);
    // TODO: Wouldn't INFINITY would be a better result than NAN?
    CHECK (std::isnan (iv.linear_combination (INFINITY)));
    CHECK (std::isnan (iv.linear_combination (NAN)));
  }

  {
    const Interval iv {-1, 9};
    EQUAL (iv.linear_combination (-INFINITY), -INFINITY);
    EQUAL (iv.linear_combination (-2), -6);
    EQUAL (iv.linear_combination (-1), -1);
    EQUAL (iv.linear_combination (0), 4);
    EQUAL (iv.linear_combination (1), 9);
    EQUAL (iv.linear_combination (2), 14);
    EQUAL (iv.linear_combination (INFINITY), INFINITY);
    CHECK (std::isnan (iv.linear_combination (NAN)));
  }
}

TEST (Interval_test, inverse_linear_combination)
{
  // point: zero length
  {
    const Interval iv {42};
    // left of point -> -INFINITY
    EQUAL (iv.inverse_linear_combination (-INFINITY), -INFINITY);
    EQUAL (iv.inverse_linear_combination (-1), -INFINITY);
    EQUAL (iv.inverse_linear_combination (0), -INFINITY);
    EQUAL (iv.inverse_linear_combination (41), -INFINITY);
    // on point
    CHECK (std::isnan (iv.inverse_linear_combination (42)));
    // right of point -> +INFINITY
    EQUAL (iv.inverse_linear_combination (43), INFINITY);
    EQUAL (iv.inverse_linear_combination (INFINITY), INFINITY);

    CHECK (std::isnan (iv.inverse_linear_combination (NAN)));
  }

  {
    const Interval iv {-1, 9};
    EQUAL (iv.inverse_linear_combination (-INFINITY), -INFINITY);
    EQUAL (iv.inverse_linear_combination (-6), -2);
    EQUAL (iv.inverse_linear_combination (-1), -1);
    EQUAL (iv.inverse_linear_combination (4), 0);
    EQUAL (iv.inverse_linear_combination (9), 1);
    EQUAL (iv.inverse_linear_combination (14), 2);
    EQUAL (iv.inverse_linear_combination (INFINITY), INFINITY);
    CHECK (std::isnan (iv.inverse_linear_combination (NAN)));
  }

  // other weird cases
  for (const auto &iv : {
         Interval {},
         Interval {-INFINITY, -INFINITY},
         Interval {-INFINITY, 0},
         Interval {-INFINITY, INFINITY},
         Interval {0, INFINITY},
         Interval {INFINITY, INFINITY},
         Interval {NAN, 0},
         Interval {0, NAN},
         Interval {NAN, NAN},
       })
    {
      CHECK (std::isnan (iv.inverse_linear_combination (-INFINITY)));
      CHECK (std::isnan (iv.inverse_linear_combination (-1)));
      CHECK (std::isnan (iv.inverse_linear_combination (0)));
      CHECK (std::isnan (iv.inverse_linear_combination (1)));
      CHECK (std::isnan (iv.inverse_linear_combination (INFINITY)));
      CHECK (std::isnan (iv.inverse_linear_combination (NAN)));
    }
}
