/*
  meter.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"
#include "varray.hh"

struct Meter: Item {
    Array<Scalar> args;
    /****************/
    
    Meter(Array<Scalar> args) ;
    Molecule*brew_molecule() const;
};
#endif // METER_HH

