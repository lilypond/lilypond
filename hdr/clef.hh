/*
  clef.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEF_HH
#define CLEF_HH
#include "scalar.hh"
#include "varray.hh"

/// where is c-0 in the staff?
class Clef {

public:
    int c0_position_i_;
    String clef_type_str_;

    Clef();
    void set_type(String);
};
#endif // CLEF_HH

