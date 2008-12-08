/*
  leastsquare.hh -- part of GNU LilyPond

  (c) 1996--2008 Han-Wen Nienhuys
*/

#ifndef LEASTSQUARE_HH
#define LEASTSQUARE_HH
#include "std-vector.hh"
#include "offset.hh"

/**
   Least squares minimisation in 2 variables.
*/
void minimise_least_squares (Real *coef, Real *offset, vector<Offset> const &);

#endif // LEASTSQUARE_HH

