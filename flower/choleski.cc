/*
  choleski.cc -- implement Choleski_decomposition

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "choleski.hh"
const Real EPS = 1e-7;		// so sue me. Hard coded

// for testing new Matrix_storage.
//#define PARANOID

void
Choleski_decomposition::full_matrix_solve (Vector &out, Vector const &rhs) const
{
  int n= rhs.dim();
  assert (n == L.dim());
  Vector y;
  y.set_dim (n);
  out.set_dim (n);
  
  // forward substitution
  for (int i=0; i < n; i++) 
    {
      Real sum (0.0);
      for (int j=0; j < i; j++)
	sum += y (j) * L(i,j);
      y (i) = (rhs (i) - sum)/L(i,i);
    }
  
  for (int i=0; i < n; i++)
    y (i) /= D(i);

  // backward subst
  Vector &x (out);		// using input as return val.
  for (int i=n-1; i >= 0; i--) 
    {
      Real sum (0.0);
      for (int j=i+1; j < n; j++)
	sum += L(j,i)*x (j);
      x (i) = (y (i) - sum)/L(i,i);
    }
}

void
Choleski_decomposition::band_matrix_solve (Vector &out, Vector const &rhs) const
{
  int n= rhs.dim();
  int b = L.band_i();
  assert (n == L.dim());

  out.set_dim (n);
  
  Vector y;
  y.set_dim (n);
  
  // forward substitution
  for (int i=0; i < n; i++) 
    {
      Real sum (0.0);
      for (int j= 0 >? i - b; j < i; j++)
	sum += y (j) * L(i,j);
      y (i) = (rhs (i) - sum)/L(i,i);
    }
  for (int i=0; i < n; i++)
    y (i) /= D(i);

  // backward subst
  Vector &x (out);		// using input as return val.
  for (int i=n-1; i >= 0; i--) 
    {
      Real sum (0.0);
      for (int j=i+1; j <= i + b&&j < n ; j++)
	sum += L(j,i)*x (j);
      x (i) = (y (i) - sum)/L(i,i);
    }
}

void
Choleski_decomposition::solve (Vector &x, Vector const &rhs) const
{
  if (L.band_b()) 
    {
      band_matrix_solve (x,rhs);
    }
  else
    full_matrix_solve (x,rhs);
}

Vector
Choleski_decomposition::solve (Vector rhs) const
{
  Vector r;
  solve (r, rhs);
  return r;
}

void
Choleski_decomposition::full_matrix_decompose (Matrix const & P)
{
 
  int n = P.dim();
  L.unit();
  for (int k= 0; k < n; k++) 
    {
      for (int j = 0; j < k; j++)
	{
	  Real sum (0.0);
	  for (int l=0; l < j; l++)
	    sum += L(k,l)*L(j,l)*D(l);
	  L(k,j) = (P(k,j) - sum)/D(j);
	}
      Real sum=0.0;
	
      for (int l=0; l < k; l++)
	sum += sqr (L(k,l))*D(l);
      Real d = P(k,k) - sum;
      D(k) = d;
    }

}

void
Choleski_decomposition::band_matrix_decompose (Matrix const &P)
{
  int n = P.dim();
  int b = P.band_i();
  L.unit();
  
  for (int i= 0; i < n; i++) 
    {
      for (int j = 0 >? i - b; j < i; j++)
	{
	  Real sum (0.0);
	  for (int l=0 >? i - b; l < j; l++)
	    sum += L(i,l)*L(j,l)*D(l);
	  L(i,j) = (P(i,j) - sum)/D(j);
	}
      Real sum=0.0;
	
      for (int l=0 >? i - b; l < i; l++)
	sum += sqr (L(i,l))*D(l);
      Real d = P(i,i) - sum;
      D(i) = d;
    }
  L.try_set_band();
  assert (L.band_i() == P.band_i ());
}




/*
  Standard matrix algorithm.
   */

Choleski_decomposition::Choleski_decomposition (Matrix const & P)
  : L(P.dim()), D(P.dim ())
{ 
#ifdef PARANOID
  assert ((P-P.transposed()).norm ()/P.norm () < EPS);
#endif
  if  (P.band_b()) 
    band_matrix_decompose (P);
  else
    full_matrix_decompose (P);
 

#ifdef PARANOID
  assert ((original()-P).norm () / P.norm () < EPS);
#endif
}

Matrix
Choleski_decomposition::original() const
{
  Matrix T(L.dim());
  T.set_diag (D);
  return L*T*L.transposed();
}

Matrix
Choleski_decomposition::inverse() const
{
  int n=L.dim();
  Matrix invm (n);
  Vector e_i (n);
  Vector inv (n);
  for (int i = 0; i < n; i++) 
    {
      e_i.set_unit (i);
      solve (inv, e_i);
      for (int j = 0 ; j<n; j++)
	invm (i,j) = inv (j);
    }
  
#ifdef PARANOID
  Matrix I1(n), I2(original());
  I1.unit();
  assert ((I1-I2*invm).norm()/I2.norm () < EPS);
#endif
  
  return invm;
}
