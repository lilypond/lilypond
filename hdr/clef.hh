/*
  clef.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef CLEF_HH
#define CLEF_HH
#include "scalar.hh"
#include "vray.hh"

struct Clef {
    int c0_pos;
    String clef_type;

    Clef();
    void read(svec<Scalar> args);    
};
#endif // CLEF_HH

