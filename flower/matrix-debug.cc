/*
  matrix-debug.cc -- implement Matrix print routines

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "flower-debug.hh"
#include "matrix.hh"
#include "matrix-storage.hh"


Matrix::operator String() const
{
  String s;
#ifndef NPRINT
  Matrix_storage const * stor_c_l = dat;
  s=String ("matrix { (")  + dat->name() + ")\n";
  for (int i=0; i< rows(); i++)
    {
      for (int j = 0; j < cols(); j++) 
	{
	  s+= String (stor_c_l->elem (i,j), "%6f ");
	}
      s+="\n";
    }
  s+="}\n";
#endif
  return s;
}


void
Matrix::print() const
{
#ifndef NPRINT
  fdebug << *this;
#endif
}

Vector::operator String() const
{
  String s;
#ifndef NPRINT
  s=String ("vector (") + dim () + ") [";
  for (int i=0; i < dim(); i++) 
    {
      s += String (dat[i], "%6f") + String (' ');
    }
  s+="]";
#endif
  return s;
}


void
Vector::print() const
{
#ifndef NDEBUG
  fdebug << *this<<'\n';
#endif
}
