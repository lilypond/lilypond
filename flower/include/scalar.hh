/*
  scalar.hh -- declare Scalar

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCALAR_HH
#define SCALAR_HH

#include "string.hh"
#include "real.hh"

/// Perl -like scalar type.
struct Scalar : public String {
    
  Scalar (Real r) : String (r) {}
  Scalar (int i) : String (i) {}
  Scalar (char c) : String (c) {}
  Scalar (char const *c) : String (c) {}    
  Scalar (String s):String (s) {}
  Scalar (Rational);
  operator Rational();
  Scalar() {}
  bool isnum_b() const;
  operator Real();
  operator int();
  bool to_bool () const;

  /**   perl -like string to bool conversion.
   */
  operator bool() const;
};

#endif // SCALAR_HH

