/*
  meter.hh -- declare Meter

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"
#include "varray.hh"
#include "scalar.hh"

/**
  TODO:

  C style meters, 2+3+2/8 meters, alla breve.
  
 */
class Meter: public Item {
    Array<Scalar> args;
    /* *************** */
protected:
    Molecule*brew_molecule_p() const;
public:
    Meter(Array<Scalar> args) ;
    NAME_MEMBERS();
    SCORE_ELEM_CLONE(Meter);
};
#endif // METER_HH

