#include "matrix.hh"

Real
Matrix::norm() const
{
    Real r =0.0;
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
  	r += sqr(dat->elem(i,j));
    return sqrt(r);
}

//inline
Real
Matrix::operator()(int i,int j) const
{
    assert(i >= 0 && j >= 0);
    assert(i < rows() && j < cols());
    return dat->elem(i,j);
}

//inline
Real &
Matrix::operator()(int i, int j)
{
    assert(i >= 0 && j >= 0);
    assert(i < rows() && j < cols());
    return dat->elem(i,j);
}

void
Matrix::fill(Real r)
{
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
	dat->elem(i,j)=r;
}

void
Matrix::set_diag(Real r)
{
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))	  
 	dat->elem(i,j)=(i==j) ? r: 0.0;
}

void
Matrix::set_diag(Vector d)
{
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))	  
 	dat->elem(i,j)=(i==j) ? d(i): 0.0;
}

void
Matrix::operator+=(const Matrix&m)
{
    assert(m.cols() == cols());
    assert(m.rows() == rows());
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
  	dat->elem(i,j) += m(i,j);
}
 
void
Matrix::operator-=(const Matrix&m)
{
    assert(m.cols() == cols());
    assert(m.rows() == rows());
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
  	dat->elem(i,j) -= m(i,j);
}


void
Matrix::operator*=(Real a)
{
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
	dat->elem(i,j) *= a;
}

void
Matrix::operator=(const Matrix&m)
{
    if (&m == this)
	return ;
    delete dat;
    dat = m.dat->clone();
}
    
Matrix::Matrix(const Matrix &m)
{
    m.OK();
    
    dat = m.dat->clone();
}


Matrix::Matrix(int n, int m)
{
    dat = virtual_smat::get_full(n,m);
    fill(0);
}

Matrix::Matrix(int n)
{
    dat = virtual_smat::get_full(n,n);
    fill(0);
}

Matrix::Matrix(Vector v, Vector w)
{   
    dat = virtual_smat::get_full(v.dim(), w.dim());
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
	dat->elem(i,j)=v(i)*w(j);
}


Vector
Matrix::row(int k) const
{
    int n=cols();

    
    Vector v(n);
    for(int i=0; i < n; i++)
	v(i)=dat->elem(k,i);

    return v;
}

Vector
Matrix::col(int k) const
{
    int n=rows();
    Vector v(n);
    for(int i=0; i < n; i++)
	v(i)=dat->elem(i,k);
    return v;
}

Vector
Matrix::left_multiply(const Vector& v) const
{
     Vector dest(v.dim());
    assert(dat->cols()==v.dim());
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
	dest(i)+= dat->elem(j,i)*v(j);
    return dest;
}

Vector
Matrix::operator *(const Vector& v) const
{
    Vector dest(rows());
    assert(dat->cols()==v.dim());
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j))
	dest(i)+= dat->elem(i,j)*v(j);
    return dest;
}

Matrix
operator /(Matrix const& m1,Real a)
{
    Matrix m(m1);
    m /= a;
    return  m;
}

void
Matrix::transpose()		// delegate to storage?
{
    for (int i=0, j=0; dat->mult_ok(i,j); dat->mult_next(i,j)) {
	if (i >= j)
	    continue;
	Real r=dat->elem(i,j);
	dat->elem(i,j) = dat->elem(j,i);
	dat->elem(j,i)=r;
    }
}

Matrix
Matrix::operator-() const
{
    OK();
    Matrix m(*this);
    m*=-1.0;    
    return m;
}

Matrix
Matrix::transposed() const
{
    Matrix m(*this);
    m.transpose();
    return m;
}


/* should do something smarter: bandmatrix * bandmatrix is also banded matrix.  */
Matrix
operator *(const Matrix &m1, const Matrix &m2)
{
    Matrix result(m1.rows(), m2.cols());
    result.set_product(m1,m2);
    return result;
}

void
Matrix::set_product(const Matrix &m1, const Matrix &m2)
{
    assert(m1.cols()==m2.rows());
    assert(cols()==m2.cols() && rows()==m1.rows());
    
    for (int i=0, j=0; dat->mult_ok(i,j);
	 dat->mult_next(i,j)) {
	Real r=0.0;
	for (int k = 0; k < m1.cols(); k++)
	    r += m1(i,k)*m2(k,j);
	dat->elem(i,j)=r;
    }
}

void
Matrix::insert_row(Vector v, int k)
{
    assert(v.dim()==cols());
    dat->insert_row(k);
    for (int j=0; j < cols(); j++)
	dat->elem(k,j)=v(j);
}


void
Matrix::swap_columns(int c1, int c2)
{
    assert(c1>=0&& c1 < cols()&&c2 < cols() && c2 >=0);
    for (int i=0; i< rows(); i++) {
	Real r=dat->elem(i,c1);
	dat->elem(i,c1) = dat->elem(i,c2);
	dat->elem(i,c2)=r;
    }
}

void
Matrix::swap_rows(int c1, int c2)
{
    assert(c1>=0&& c1 < rows()&&c2 < rows() && c2 >=0);
    for (int i=0; i< cols(); i++) {
	Real r=dat->elem(c1,i);
	dat->elem(c1,i) = dat->elem(c2,i);
	dat->elem(c2,i)=r;
    }
}


int
Matrix::dim() const
{
    assert(cols() == rows());
    return rows();
}

