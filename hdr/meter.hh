/*
  meter.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"
#include "vray.hh"

struct Meter: Item {
    svec<Scalar> args;
    
    Meter(svec<Scalar> args) ;
    void preprocess();
};
#endif // METER_HH

