/*
  time_signature.hh -- declare Time_signature

  (c) 1996--2001 Han-Wen Nienhuys
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
  static Molecule special_time_signature (Grob*,String,int,int) ;
  static Molecule time_signature (Grob*,int, int);
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
};
#endif // METER_HH

