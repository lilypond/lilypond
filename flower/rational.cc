/*
  rational.cc -- implement Rational related functions

  source file of the Flower Library

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rational.hh"
#include "string.hh"

void
print_rat(Rational const &m)
{
    cout << String(m) << flush;
}
    

