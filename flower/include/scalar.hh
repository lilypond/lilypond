/*
  scalar.hh -- declare Scalar

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCALAR_HH
#define SCALAR_HH

#include "string.hh"
#include "real.hh"

/// Perl -like scalar type.
struct Scalar : public String 
{
  Scalar (Real r) { *this = to_str (r); }
  Scalar (int i) { *this = to_str (i); }
  Scalar (long l) { *this = to_str (l); }
  Scalar (char c) { *this = to_str (c); }
  Scalar (char const *c) : String (c) {}    
  Scalar (String s) : String (s) {}
  Scalar (Rational);
  operator Rational();
  Scalar() {}
  bool isnum_b() const;
  bool isdir_b() const;
  bool isint_b() const;
  operator Real();
  operator int();
  bool to_bool () const;
  Rational to_rat () const;
  int to_i () const;
  Real to_f () const;


  /**   perl -like string to bool conversion.
   */
  operator bool() const;
};

#endif // SCALAR_HH

