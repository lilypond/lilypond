/*
  time_signature.hh -- declare Time_signature

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef METER_HH
#define METER_HH

#include "item.hh"

/**
   Print a time_signature sign.

   TODO:

   2+3+2/8 time_signatures
  
 */
struct Time_signature
{
  static Molecule special_time_signature (Score_element*,String,int,int) ;
  static Molecule time_signature (Score_element*,int, int);
  static SCM brew_molecule (SCM);
};
#endif // METER_HH

