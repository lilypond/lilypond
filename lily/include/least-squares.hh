/*
  leastsquare.hh -- part of GNU LilyPond

  (c) 1996--2001 Han-Wen Nienhuys
*/

#ifndef LEASTSQUARE_HH
#define LEASTSQUARE_HH
#include "array.hh"
#include "offset.hh"


/**
  Least squares minimisation in 2 variables.
  */
void minimise_least_squares (Real * coef, Real * offset, Array<Offset>);

#endif // LEASTSQUARE_HH

