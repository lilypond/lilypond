/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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

#ifndef MATRIX_HH
#define MATRIX_HH

#include <cassert>
#include <vector>

template <class T, class A = std::allocator<T>>
class Matrix
{
  using Vector = std::vector<T, A>;
  using size_type = typename Vector::size_type;

public:
  Matrix<T, A> () { rank_ = 0; }

  Matrix<T, A> (size_type rows, size_type columns, T const &t)
    : data_ (rows * columns, t)
  {
    rank_ = rows;
  }

  const T &at (size_type row, size_type col) const
  {
    assert (row < rank_ && col * rank_ + row < data_.size ());

    return data_[col * rank_ + row];
  }

  T &at (size_type row, size_type col)
  {
    assert (row < rank_ && col * rank_ + row < data_.size ());

    return data_[col * rank_ + row];
  }

  void resize (size_type rows, size_type columns, T const &t)
  {
    if (rows == rank_)
      data_.resize (rows * columns, t);
    else
      {
        Vector new_data;
        new_data.resize (rows * columns, t);
        size_type cur_cols = rank_ ? data_.size () / rank_ : 0;

        for (size_type i = 0; i < cur_cols; i++)
          for (size_type j = 0; j < rank_; j++)
            new_data[i * rows + j] = data_[i * rank_ + j];
        rank_ = rows;
        data_ = new_data;
      }
  }

private:
  Vector data_;
  size_type rank_;
};

#endif /* MATRIX_HH */
