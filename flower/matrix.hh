#ifndef MATRIX_HH
#define MATRIX_HH


#include "vsmat.hh"
#include "vector.hh"

/// a Real matrix
/** This is a class for a nonsquare block of #Real#s.  The
    implementation of sparse matrices is done in the appropriate #smat#
    class. Matrix only does the mathematical actions (adding,
    multiplying, etc.)

    
    TODO
    implement ref counting?  */


class Matrix {
    virtual_smat *dat;
    
public:
    void OK() const { dat->OK(); }
    int cols() const { return dat->cols(); }
    int rows() const { return dat->rows(); }

    /// return the size of a matrix
    /**
      @pre
      the matrix needs to be square.
    */
    int dim() const;
     
    // Matrix() { dat = 0; } 
    ~Matrix() { delete dat; }

    /// set entries to r 
    void fill(Real r);

    /// set diagonal to d
    void set_diag(Real d);

    void set_diag(Vector d);
    /// set unit matrix
    void unit() { set_diag(1.0); }

    void operator+=(const Matrix&m);
    void operator-=(const Matrix&m);    
    void operator*=(Real a);
    void operator/=(Real a) { (*this) *= 1/a; }
    
    /// add a row
    /**
      add a row to the matrix before  row k

      PRE
      v.dim() == cols()
      0 <= k <= rows()
    */
    void insert_row(Vector v,int k);
    ///
    /**
      delete a row from this matrix.

      PRE
      0 <= k < rows();
    */
    void delete_row(int k) { dat->delete_row(k); }
    void delete_column(int k) { dat->delete_column(k); }

    /**
      square n matrix, initialised to null
    */
    Matrix(int n);

    /**
      n x m matrix, init to 0
    */
    Matrix(int n, int m);
    Matrix(const Matrix &m);

    /// dyadic product: v * w.transpose
    Matrix(Vector v, Vector w);
    void operator=(const Matrix&m);

    /// access an element
    Real operator()(int i,int j) const { return dat->elem(i,j); }

    /// access an element
    Real &operator()(int i, int j) { return dat->elem(i,j); }

    /// Matrix multiply with vec (from right)
    Vector operator *(const Vector &v) const;

    /// set this to m1*m2.
    void set_product(const Matrix &m1, const Matrix &m2);


    Vector left_multiply(Vector const &) const;
    
    Matrix operator-() const;
    
    /// transpose this.
    void transpose();
    
    /// return a transposed copy.
    Matrix transposed() const ;

    Real norm() const;
    /// swap
    /**
      PRE
      0 <= c1,c2 < cols()
    */
    void swap_columns(int c1, int c2);

    /// swap
    /**
      PRE
      0 <= c1,c2 < rows()
    */
    void swap_rows(int c1, int c2);


    Vector row(int ) const;
    Vector col(int) const;

    operator String() const;
    void print() const;
};

inline Vector
operator *(Vector &v, const Matrix& m) { return m.left_multiply(v); }
Matrix operator *(const Matrix& m1,const Matrix &m2);
Matrix operator /(const Matrix &m1,Real a);
inline Matrix operator -(Matrix m1,const Matrix m2)
{
    m1 -= m2;
    return m1;
}
#endif
