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
    Scalar(Rational );
    operator Rational();
    Scalar() {}
    bool isnum();
    operator Real();
    operator int();
    ///
    /** perl -like string to bool conversion
     */
    operator bool() const;

};

#endif // SCALAR_HH

