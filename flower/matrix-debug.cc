/*
  matrix-debug.cc -- implement Matrix print routines

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "flower-debug.hh"
#include "matrix.hh"

String
Matrix::str () const
{
  String s;
#ifndef NPRINT
  Full_storage const * stor_c_l = dat_;
  s = String ("matrix {");
  for (int i=0; i< rows(); i++)
    {
      for (int j = 0; j < cols(); j++) 
	{
	  s+= to_str (stor_c_l->elem (i,j), "%6f ");
	}
      s+="\n";
    }
  s+="}\n";
#endif
  return s;
}


void
Matrix::print () const
{
#ifndef NPRINT
  fdebug << *this;
#endif
}

String
Vector::str () const
{
  String s;
#ifndef NPRINT
  s = String ("vector (") + to_str (dim ()) + ") [";
  for (int i=0; i < dim(); i++) 
    {
      s += to_str (dat[i], "%6f") + to_str (' ');
    }
  s += "]\n";
#endif
  return s;
}


void
Vector::print() const
{
#ifndef NDEBUG
  fdebug << *this << '\n';
#endif
}
