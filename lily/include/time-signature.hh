/*
  time_signature.hh -- declare Time_signature

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH
#include "item.hh"
#include "array.hh"
#include "scalar.hh"

/**
   Print a time_signature sign.
  TODO:

  C style time_signatures, 2+3+2/8 time_signatures, alla breve.
  
 */
class Time_signature: public Item {

  

protected:
  virtual Molecule*brew_molecule_p() const;
public:
  Time_signature () ;
  Array<Scalar> args_;
  String time_sig_type_str_;
  
  DECLARE_MY_RUNTIME_TYPEINFO;
  SCORE_ELEMENT_CLONE(Time_signature);
};
#endif // METER_HH

