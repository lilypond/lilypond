/*
  meter.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"

struct Meter: Item {
    svec<String> args;
    
    Meter(svec<String> args) ;
    void preprocess();
};
#endif // METER_HH

