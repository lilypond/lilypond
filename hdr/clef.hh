/*
  clef.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef CLEF_HH
#define CLEF_HH
#include "string.hh"
#include "vray.hh"

struct Clef {
    int c0_pos;
    String clef_type;

    Clef();
    void read(svec<String> args);    
};
#endif // CLEF_HH

