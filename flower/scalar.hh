/*
  scalar.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SCALAR_HH
#define SCALAR_HH
#include "string.hh"
#include "real.hh"

/// Perl -like scalar type.
struct Scalar : public String {
    
    Scalar(Real r) : String(r) {}
    Scalar(int i) : String(i) {}
    Scalar(char c) : String(c) {}
    Scalar(const char *c) : String(c) {}    
    Scalar(String s ):String(s) {}
    Scalar() {}
    bool isnum();
    operator Real();
    operator int();
    ///
    bool to_bool() const;
    /** perl -like string to bool conversion
     */

};

#endif // SCALAR_HH

