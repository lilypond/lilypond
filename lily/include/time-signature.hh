/*
  time_signature.hh -- declare Time_signature

  (c) 1996--2000 Han-Wen Nienhuys
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
class Time_signature: public Item
{
  Molecule special_time_signature (String,int,int) const;
  Molecule time_signature (int, int)const;
  
protected:
  virtual Molecule do_brew_molecule() const;
public:
  Time_signature (SCM);
 static SCM scheme_molecule (SCM);
  

  /*
    TODO: make this SCM!
   */
  Array<int> args_;
  
  VIRTUAL_COPY_CONS(Score_element);
};
#endif // METER_HH

