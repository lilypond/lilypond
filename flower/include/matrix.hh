/*
  matrix.hh -- declare and implement 2d arrays

  source file of the Flower Library

  (c) 2006--2008 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef MATRIX_HH
#define MATRIX_HH

#include "std-vector.hh"

template<class T, class A=std::allocator<T> >
class Matrix
{
public:
  Matrix<T, A> ()
  {
    rank_ = 0;
  }

  Matrix<T, A> (vsize rows, vsize columns, T const &t)
  : data_(rows * columns, t)
  {
    rank_ = rows;
  }

  const T &at (vsize row, vsize col) const
  {
    assert (row < rank_ && col * rank_ + row < data_.size ());

    return data_[col * rank_ + row];
  }

  T &at (vsize row, vsize col)
  {
    assert (row < rank_ && col * rank_ + row < data_.size ());

    return data_[col * rank_ + row];
  }

  void resize (vsize rows, vsize columns, T const &t)
  {
    if (rows == rank_)
      data_.resize (rows * columns, t);
    else
      {
	vector<T,A> new_data;
	new_data.resize (rows * columns, t);
	vsize cur_cols = rank_ ? data_.size () / rank_: 0;

	for (vsize i = 0; i < cur_cols; i++)
	  for (vsize j = 0; j < rank_; j++)
	    new_data[i*rows + j] = data_[i*rank_ + j];
	rank_ = rows;
	data_ = new_data;
      }
  }

private:
  vector<T, A> data_;
  vsize rank_;
};

#endif /* MATRIX_HH */
