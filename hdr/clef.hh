/*
  clef.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef CLEF_HH
#define CLEF_HH
#include "scalar.hh"
#include "varray.hh"

struct Clef {
    int c0_pos;
    String clef_type;

    Clef();
    void read(Array<Scalar> args);    
};
#endif // CLEF_HH

