#ifndef VECTOR_HH
#define VECTOR_HH

#include "glob.hh"
#include "vray.hh"

/// a row of numbers
class Vector  {
    svec<Real> dat;
public:
    void OK() const { dat.OK();}
    int dim() const { return dat.sz(); }
    Vector() { }
    Vector(const Vector&n);
    Vector(int n) {
	dat.set_size(n);
	fill(0);
    }
    void insert(Real v, int i) {
	dat.insert(v,i);
    }
    void del(int i) { dat.del(i); }
    operator String() const;
    void fill(Real r) {
	for (int i=0; i < dim(); i++)
	    dat[i] =r;
    }

    void operator +=(Vector v) {
	assert(v.dim() == dim());
	for (int i=0; i < dim(); i++)
	    dat[i] += v.dat[i];
    }
    
    void operator /=(Real a) {
	(*this) *= 1/a;
    }

    void operator *=(Real a) {
	for (int i=0; i < dim(); i++)
	    dat[i] *= a;
    }

    void operator -=(Vector v) {
	assert(v.dim() == dim());
	for (int i=0; i < dim(); i++)
	    dat[i] -= v(i);	
    }

    Real &operator()(int i) { return dat[i]; }
    Real operator()(int i) const { return dat[i]; }
    Real elem(int i) { return dat[i]; }
    Real operator *(Vector v) const {
	Real ip=0;
	assert(v.dim() == dim());
	for (int i=0; i < dim(); i++)
	    ip += dat[i] *v(i);
	return ip;
    }
    Vector operator-() const;
    Real norm() {
	return sqrt(norm_sq() );
    }
    Real norm_sq() {
	return ((*this) * (*this));
    }
    operator svec<Real> () { return dat; }
    void print() const;
    /// set to j-th element of unit-base
    void set_unit(int j) ;
};
/**
    a vector. Storage is handled in svec, Vector only does the mathematics.
 */

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
operator*( Real a,Vector v) {
    v *= a;
    return v;
}

inline Vector
operator/(Vector v,Real a) {
    v *= 1/a;
    return v;
}

#endif
