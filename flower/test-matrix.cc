/*
  This file is part of LilyPond, the GNU music typesetter.

  P22--2023 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "matrix.hh"

#include "yaffut.hh"

class Matrix_test
{
};

TEST (Matrix_test, create_default)
{
  auto m = Matrix<int> ();

  static_cast<void> (m);
}

TEST (Matrix_test, create_0_by_0)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (0, 0, X);

  static_cast<void> (m);
}

TEST (Matrix_test, create_2_by_2)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (2, 2, X);

  EQUAL (X, m.at (0, 0));
  EQUAL (X, m.at (0, 1));
  EQUAL (X, m.at (1, 0));
  EQUAL (X, m.at (1, 1));
}

TEST (Matrix_test, resize_increase_rows)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (2, 2, X);

  m.at (0, 0) = 0;
  m.at (0, 1) = 1;
  m.at (1, 0) = 2;
  m.at (1, 1) = 3;

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));

  m.resize (3, 2, X);

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));
  EQUAL (X, m.at (2, 0));
  EQUAL (X, m.at (2, 1));
}

TEST (Matrix_test, resize_increase_cols)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (2, 2, X);

  m.at (0, 0) = 0;
  m.at (0, 1) = 1;
  m.at (1, 0) = 2;
  m.at (1, 1) = 3;

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));

  m.resize (2, 3, X);

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (X, m.at (0, 2));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));
  EQUAL (X, m.at (1, 2));
}

TEST (Matrix_test, resize_reduce_rows)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (2, 2, X);

  m.at (0, 0) = 0;
  m.at (0, 1) = 1;
  m.at (1, 0) = 2;
  m.at (1, 1) = 3;

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));

  // Matrix doesn't provide a convenient way to check that its size was reduced;
  // the most direct feedback would be an abort from an out-of-range at().
  // Check indirectly by increasing the size after reducing it.  When a matrix
  // is enlarged, new elements have an initial value, and it should only have
  // been enlarged where it was previously reduced.
  m.resize (1, 2, X);
  m.resize (2, 2, X);

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (X, m.at (1, 0));
  EQUAL (X, m.at (1, 1));
}

TEST (Matrix_test, resize_reduce_cols)
{
  constexpr int X = 10; // fill value
  auto m = Matrix<int> (2, 2, X);

  m.at (0, 0) = 0;
  m.at (0, 1) = 1;
  m.at (1, 0) = 2;
  m.at (1, 1) = 3;

  EQUAL (0, m.at (0, 0));
  EQUAL (1, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (3, m.at (1, 1));

  // same approach as in resize_reduce_rows
  m.resize (2, 1, X);
  m.resize (2, 2, X);

  EQUAL (0, m.at (0, 0));
  EQUAL (X, m.at (0, 1));
  EQUAL (2, m.at (1, 0));
  EQUAL (X, m.at (1, 1));
}
