/*
  mat-test.cc -- test Matrix

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <iostream.h>
#include "matrix.hh"
#include "string.hh"
#include "flower-test.hh"
#include "choleski.hh"

void
matrix()
{
  int N=10;
  Matrix m(N,N), q(N,N);
  Vector v(N);

  for (int i=0; i < N; i++) {
    v(i) =i;
    for (int j=0; j < N; j++) {
      m(i,j) = i+j;
      q(i,j) = (abs(i-j) > 3) ?0 :i-j;
    }
  }

  cout << "v: " << String(v);
  cout << "m: " <<  String(m );
  cout << "q: " <<  String(q);
  cout << "m*q; " <<  String(m*q);
  cout << "m*m: " <<  String(m*m);
  m.OK();
  cout << "m: " <<  String(m);
  cout << "q.band " << q.band_i() << endl; 
  q.try_set_band();
  cout << "q(B): " << q;
  q.OK();
  Matrix sum(q+q);
  cout << "q + q " << sum;
  q.OK();
  cout << "q*q: " << q*q;
  q.OK();

  Matrix hilbert(N,N), h2(hilbert);
  for (int i=0; i < N; i++) {
    for (int j=0; j < N; j++) {
      hilbert(i,j) = 1/Real(i+j+1);
      h2 (i,j) = (abs(i-j) > 3) ?0 : hilbert(i,j);
    }
  }
  h2.try_set_band();
  Choleski_decomposition ch(h2);
  cout << "red Hilbert  " <<  h2;
  cout << "choleski " << ch.L;
  Matrix T =ch.L.transposed();
  cout << "L^T " <<  T;
  cout << "L * L^T" << ch.L * T;
  cout << "H2^{-1} * H2" << h2 * ch.inverse();
}

ADD_TEST(matrix);
