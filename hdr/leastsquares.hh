/*
  leastsquare.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef LEASTSQUARE_HH
#define LEASTSQUARE_HH
#include "vray.hh"
#include "offset.hh"

struct Least_squares {
    svec<Offset> input;
    void minimise(Real &coef, Real &offset);
};


#endif // LEASTSQUARE_HH

