#ifndef VECTOR_HH
#define VECTOR_HH

#include <math.h>
#include "real.hh"
#include "array.hh"

class Dstream;
class String;

/**  a row of numbers. 
    a vector. Storage is handled in Array, Vector only does the mathematics.
 */
class Vector  {
    Array<Real> dat;
public:
    void OK() const { dat.OK();}
    int dim() const { return dat.size (); }
    void fill (Real r) {
	for (int i=0; i < dim(); i++)
	    dat[i] =r;
    }

    Vector() { }
    Vector (Array<Real> d);
    Vector (Vector const &n);

    Real &operator()(int i) { return dat[i]; }
    Real operator()(int i) const { return dat[i]; }
    Real elem (int i) { return dat[i]; }
    Vector (int n) {
	dat.set_size (n);
	fill (0);
    }
    void set_dim (int i)
    {
	dat.set_size (i);
    }
	
    void insert (Real v, int i) {
	dat.insert (v,i);
    }
    void del (int i) { dat.del (i); }

    String str () const;

    void operator +=(Vector v) {
	assert (v.dim() == dim ());
	for (int i=0; i < dim(); i++)
	    dat[i] += v.dat[i];
    }
    
    void operator -=(Vector v) {
	assert (v.dim() == dim ());
	for (int i=0; i < dim(); i++)
	    dat[i] -= v (i);	
    }

    Real operator *(Vector v) const {
	Real ip=0;
	assert (v.dim() == dim ());
	for (int i=0; i < dim(); i++)
	    ip += dat[i] *v (i);
	return ip;
    }
    void operator *=(Real a) {
	for (int i=0; i < dim(); i++)
	    dat[i] *= a;
    }
    void operator /=(Real a) {
	(*this) *= 1/a;
    }
    Vector operator-() const;
    Real norm_sq() {
	return ((*this) * (*this));
    }
    Real norm() {
	return sqrt (norm_sq());
    }
    operator Array<Real>() { return dat; }
  Array<Real> const &to_array()const { return dat; }  
    void print() const;
    /// set to j-th element of unit-base
    void set_unit (int j) ;
};

inline Vector
operator+(Vector a, Vector const &b) {
    a += b;
    return a;
}

inline Vector
operator-(Vector a, Vector const &b) {
    a -= b;
    return a;
}

inline Vector
operator*(Vector v, Real a) {
    v *= a;
    return v;
}

inline Vector
operator*(Real a,Vector v) {
    v *= a;
    return v;
}

inline Vector
operator/(Vector v,Real a) {
    v *= 1/a;
    return v;
}

#endif
