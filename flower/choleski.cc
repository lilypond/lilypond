#include "choleski.hh"
const Real EPS = 1e-7;		// so sue me. Hard coded

Vector
Choleski_decomposition::solve(Vector rhs)const
{
    int n= rhs.dim();
    assert(n == L.dim());
    Vector y(n);

    // forward substitution
    for (int i=0; i < n; i++) {
	Real sum(0.0);
	for (int j=0; j < i; j++)
	    sum += y(j) * L(i,j);
	y(i) = (rhs(i) - sum)/L(i,i);
    }
    for (int i=0; i < n; i++)
	y(i) /= D(i);

    // backward subst
    Vector &x(rhs);		// using input as return val.
    for (int i=n-1; i >= 0; i--) {
	Real sum(0.0);
	for (int j=i+1; j < n; j++)
	    sum += L(j,i)*x(j);
	x(i) = (y(i) - sum)/L(i,i);
    }
    return x;
}

/*
  Standard matrix algorithm.
  */

Choleski_decomposition::Choleski_decomposition(Matrix P)
    : L(P.dim()), D(P.dim())
{
    int n = P.dim();
    assert((P-P.transposed()).norm()/P.norm() < EPS);

    L.unit();
    for (int k= 0; k < n; k++) {
	for (int j = 0; j < k; j++){
	    Real sum(0.0);
	    for (int l=0; l < j; l++)
		sum += L(k,l)*L(j,l)*D(l);
	    L(k,j) = (P(k,j) - sum)/D(j);
	}
	Real sum=0.0;
	
	for (int l=0; l < k; l++)
	    sum += sqr(L(k,l))*D(l);
	Real d = P(k,k) - sum;
	D(k) = d;
    }

#ifdef NDEBUG
    assert((original()-P).norm() / P.norm() < EPS);
#endif
}

Matrix
Choleski_decomposition::original() const
{
    Matrix T(L.dim());
    T.set_diag(D);
    return L*T*L.transposed();
}

Matrix
Choleski_decomposition::inverse() const
{
    int n=L.dim();
    Matrix invm(n);
    Vector e_i(n);
    for (int i = 0; i < n; i++) {
	e_i.set_unit(i);
	Vector inv(solve(e_i));
	for (int j = 0 ; j<n; j++)
	    invm(i,j) = inv(j);
    }
    
#ifdef NDEBUG
    Matrix I1(n), I2(original());
    I1.unit();
    assert((I1-original()*invm).norm()/original.norm() < EPS);
#endif
    
    return invm;
}
