/*
  matrix.cc -- implement Matrix

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "matrix.hh"
#include "full-storage.hh"
#include "diagonal-storage.hh"

bool
Matrix::band_b()const
{
    return dat->is_type_b (Diagonal_storage::static_name());
}

void
Matrix::set_full() const
{
    if ( dat->name() != Full_storage::static_name ()) {
	Matrix_storage::set_full (((Matrix*)this)->dat);
    }
}

void
Matrix::try_set_band()const
{
    if (band_b())
	return;
    
    int b = band_i();
    if (  b > dim()/2)
	return;
    // it only looks constant
    Matrix*self  = (Matrix*)this;
    Matrix_storage::set_band (self->dat,b);
}

Real
Matrix::norm() const
{
    Real r =0.0;
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
  	r += sqr (dat->elem (i,j));
    return sqrt (r);
}

void
Matrix::fill (Real r)
{
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
	dat->elem (i,j)=r;
}

void
Matrix::set_diag (Real r)
{
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))	  
 	dat->elem (i,j)=(i==j) ? r: 0.0;
}

void
Matrix::set_diag (Vector d)
{
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))	  
 	dat->elem (i,j)=(i==j) ? d (i): 0.0;
}

void
Matrix::operator+=(Matrix const &m)
{
    Matrix_storage::set_addition_result (dat, m.dat);
    assert (m.cols() == cols ());
    assert (m.rows() == rows ());
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
  	dat->elem (i,j) += m (i,j);
}
 
void
Matrix::operator-=(Matrix const &m)
{
    Matrix_storage::set_addition_result (dat, m.dat);
    assert (m.cols() == cols ());
    assert (m.rows() == rows ());
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
  	dat->elem (i,j) -= m (i,j);
}


void
Matrix::operator*=(Real a)
{
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
	dat->elem (i,j) *= a;
}

void
Matrix::operator=(Matrix const &m)
{
    if (&m == this)
	return ;
    delete dat;
    dat = m.dat->clone();
}

int
Matrix::band_i()const
{
    if ( band_b()) {
	Diagonal_storage const * diag = (Diagonal_storage*) dat;
	return diag->band_size_i();
    }
    int starty = dim();
    while (starty >= 0) {
	for ( int i = starty, j = 0; i < dim(); i++, j++)
	    if (dat->elem (i,j))
		goto gotcha;
	for ( int i=0, j = starty; j < dim(); i++,j++)
	    if (dat->elem (i,j))
		goto gotcha;
	starty --;
    }
gotcha:
    return  starty;
}
    
Matrix::Matrix (Matrix const &m)
{
    m.OK();
    
    dat = m.dat->clone();
}


Matrix::Matrix (int n, int m)
{
    dat = Matrix_storage::get_full (n,m);
    fill (0);
}

Matrix::Matrix (Matrix_storage*stor_p)
{
    dat = stor_p;
}

Matrix::Matrix (int n)
{
    dat = Matrix_storage::get_full (n,n);
    fill (0);
}

Matrix::Matrix (Vector v, Vector w)
{   
    dat = Matrix_storage::get_full (v.dim(), w.dim ());
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
	dat->elem (i,j)=v (i)*w (j);
}


Vector
Matrix::row (int k) const
{
    int n=cols();

    
    Vector v (n);
    for (int i=0; i < n; i++)
	v (i)=dat->elem (k,i);

    return v;
}

Vector
Matrix::col (int k) const
{
    int n=rows();
    Vector v (n);
    for (int i=0; i < n; i++)
	v (i)=dat->elem (i,k);
    return v;
}

Vector
Matrix::left_multiply (Vector const & v) const
{
     Vector dest (v.dim());
    assert (dat->cols()==v.dim ());
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
	dest (i)+= dat->elem (j,i)*v (j);
    return dest;
}

Vector
Matrix::operator *(Vector const & v) const
{
    Vector dest (rows());
    assert (dat->cols()==v.dim ());
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j))
	dest (i)+= dat->elem (i,j)*v (j);
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
Matrix::transpose()		// delegate to storage?
{
#if 1
    for (int i=0, j=0; dat->mult_ok (i,j); dat->mult_next (i,j)) {
	if (i >= j)
	    continue;
	Real r=dat->elem (i,j);
	dat->elem (i,j) = dat->elem (j,i);
	dat->elem (j,i)=r;
    }
#endif
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
    Matrix result (Matrix_storage::get_product_result (m1.dat, m2.dat));

    result.set_product (m1,m2);
    return result;
}

void
Matrix::set_product (Matrix const &m1, Matrix const &m2)
{
    assert (m1.cols()==m2.rows ());
    assert (cols()==m2.cols () && rows ()==m1.rows ());
    
    if (m1.dat->try_right_multiply (dat, m2.dat))
	return; 
    
    for (int i=0, j=0; dat->mult_ok (i,j);
	 dat->mult_next (i,j)) {
	Real r=0.0;
	for (int k = 0; k < m1.cols(); k++)
	    r += m1(i,k)*m2(k,j);
	dat->elem (i,j)=r;
    }
}

void
Matrix::insert_row (Vector v, int k)
{
    int c = cols();
    assert (v.dim()==cols ());
    dat->insert_row (k);
    for (int j=0; j < c; j++)
	dat->elem (k,j)=v (j);
}


void
Matrix::swap_columns (int c1, int c2)
{
    assert (c1>=0&& c1 < cols()&&c2 < cols () && c2 >=0);
    int r = rows();
    for (int i=0; i< r; i++) {
	Real r=dat->elem (i,c1);
	dat->elem (i,c1) = dat->elem (i,c2);
	dat->elem (i,c2)=r;
    }
}

void
Matrix::swap_rows (int c1, int c2)
{
    assert (c1>=0&& c1 < rows()&&c2 < rows () && c2 >=0);
    int c = cols();
    for (int i=0; i< c; i++) {
	Real r=dat->elem (c1,i);
	dat->elem (c1,i) = dat->elem (c2,i);
	dat->elem (c2,i)=r;
    }
}


int
Matrix::dim() const
{
    assert (cols() == rows ());
    return rows();
}



