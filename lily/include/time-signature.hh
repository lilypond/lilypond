/*
  time_signature.hh -- declare Time_signature

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH

#include "item.hh"
#include "array.hh"
#include "real.hh"

/**
   Print a time_signature sign.
  TODO:

  C style time_signatures, 2+3+2/8 time_signatures, alla breve.
  
 */
class Time_signature: public Item {

  

protected:
  virtual Molecule*do_brew_molecule_p() const;
public:
  Time_signature () ;
  Array<int> args_;
  String time_sig_type_str_;
  
  
  VIRTUAL_COPY_CONS(Score_element);
};
#endif // METER_HH

