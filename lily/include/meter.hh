/*
  meter.hh -- declare Meter

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"
#include "varray.hh"
#include "scalar.hh"

struct Meter: Item {
    Array<Scalar> args;
    /* *************** */
NAME_MEMBERS(Meter);
    
    Meter(Array<Scalar> args) ;
    Molecule*brew_molecule_p() const;
};
#endif // METER_HH

