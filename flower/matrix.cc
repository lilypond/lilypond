/*
  matrix.cc -- implement Matrix

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "matrix.hh"
#include "full-storage.hh"

Real
Matrix::norm() const
{
  Real r =0.0;
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    r += sqr (dat_->elem (i,j));
  return sqrt (r);
}

void
Matrix::fill (Real r)
{
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dat_->elem (i,j)=r;
}

void
Matrix::set_diag (Real r)
{
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))	  
    dat_->elem (i,j)=(i==j) ? r: 0.0;
}

void
Matrix::set_diag (Vector d)
{
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))	  
    dat_->elem (i,j)=(i==j) ? d (i): 0.0;
}

void
Matrix::operator+=(Matrix const &m)
{
  assert (m.cols() == cols ());
  assert (m.rows() == rows ());
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dat_->elem (i,j) += m (i,j);
}
 
void
Matrix::operator-=(Matrix const &m)
{
  assert (m.cols() == cols ());
  assert (m.rows() == rows ());
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dat_->elem (i,j) -= m (i,j);
}


void
Matrix::operator*=(Real a)
{
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dat_->elem (i,j) *= a;
}

void
Matrix::operator=(Matrix const &m)
{
  if (&m == this)
    return ;
  delete dat_;
  dat_ = new Full_storage (*m.dat_);
}


int
Matrix::band_i () const
{
  return dat_->band_i_;
}

void
Matrix::set_band ()
{
  dat_->band_i_ = calc_band_i ();
}

int
Matrix::calc_band_i() const
{
  int starty = dim();
  while (starty >= 0) 
    {
      for (int i = starty, j = 0; i < dim(); i++, j++)
	if (dat_->elem (i,j))
	  goto gotcha;
      for (int i=0, j = starty; j < dim(); i++,j++)
	if (dat_->elem (i,j))
	  goto gotcha;
      starty --;
    }
 gotcha:
  return  starty;
}
  
Matrix::Matrix (Matrix const &m)
{
  dat_ = new Full_storage (*m.dat_);
}


Matrix::Matrix (int n, int m)
{
  dat_ = new Full_storage(n,m);
  fill (0);
}


Matrix::Matrix (int n)
{
  dat_= new Full_storage (n,n);
  fill (0);
}

Matrix::Matrix (Vector v, Vector w)
{
  int n = v.dim();
  int m = w.dim ();
  dat_= new Full_storage (n,m);
  for (int i=0; i < n; i++)
    for (int j=0; j < m ; j++)
      dat_->elem (i,j)=v (i)*w (j);
}


Vector
Matrix::row (int k) const
{
  int n=cols();

  
  Vector v (n);
  for (int i=0; i < n; i++)
    v (i)=dat_->elem (k,i);

  return v;
}

Vector
Matrix::col (int k) const
{
  int n=rows();
  Vector v (n);
  for (int i=0; i < n; i++)
    v (i)=dat_->elem (i,k);
  return v;
}

Vector
Matrix::left_multiply (Vector const & v) const
{
  Vector dest (v.dim());
  assert (dat_->cols()==v.dim ());
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dest (i)+= dat_->elem (j,i)*v (j);
  return dest;
}

Vector
Matrix::operator *(Vector const & v) const
{
  Vector dest (rows());
  assert (dat_->cols()==v.dim ());
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j))
    dest (i)+= dat_->elem (i,j)*v (j);
  return dest;
}

Matrix
operator /(Matrix const& m1,Real a)
{
  Matrix m (m1);
  m /= a;
  return  m;
}

/*
  ugh. Only works for square matrices.
 */
void
Matrix::transpose()
{
  for (int i=0, j=0; dat_->mult_ok (i,j); dat_->mult_next (i,j)) 
    {
      if (i >= j)
	continue;
      Real r=dat_->elem (i,j);
      dat_->elem (i,j) = dat_->elem (j,i);
      dat_->elem (j,i)=r;
    }
}

Matrix
Matrix::operator-() const
{
  OK();
  Matrix m (*this);
  m*=-1.0;    
  return m;
}

Matrix
Matrix::transposed() const
{
  Matrix m (*this);
  m.transpose();
  return m;
}

Matrix
operator *(Matrix const &m1, Matrix const &m2)
{
  Matrix result (m1.rows (), m2.cols ());
  result.set_product (m1,m2);
  return result;
}



void
Matrix::set_product (Matrix const &m1, Matrix const &m2)
{
  assert (m1.cols()==m2.rows ());
  assert (cols()==m2.cols () && rows ()==m1.rows ());
  
  for (int i=0; i < rows (); i++)
    for (int j=0; j <  cols (); j++)
      {
	Real r=0.0;
	for (int k = 0 >? i - m1.band_i () >? j - m2.band_i ();
	     k < m1.cols(); k++)
	  {
	    r += m1(i,k)*m2(k,j);
	  }
	dat_->elem (i,j)=r;
      }
}

void
Matrix::insert_row (Vector v, int k)
{
  int c = cols();
  assert (v.dim()==cols ());
  dat_->insert_row (k);
  for (int j=0; j < c; j++)
    dat_->elem (k,j)=v (j);
}


void
Matrix::swap_columns (int c1, int c2)
{
  assert (c1>=0&& c1 < cols()&&c2 < cols () && c2 >=0);
  int r = rows();
  for (int i=0; i< r; i++) 
    {
      Real r=dat_->elem (i,c1);
      dat_->elem (i,c1) = dat_->elem (i,c2);
      dat_->elem (i,c2)=r;
    }
}

void
Matrix::swap_rows (int c1, int c2)
{
  assert (c1>=0&& c1 < rows()&&c2 < rows () && c2 >=0);
  int c = cols();
  for (int i=0; i< c; i++) 
    {
      Real r=dat_->elem (c1,i);
      dat_->elem (c1,i) = dat_->elem (c2,i);
      dat_->elem (c2,i)=r;
    }
}


int
Matrix::dim() const
{
  assert (cols() == rows ());
  return rows();
}

Matrix::~Matrix ()
{
  delete dat_;
}
