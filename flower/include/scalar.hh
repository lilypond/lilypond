/*
  scalar.hh -- declare Scalar

  source file of the Flower Library

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SCALAR_HH
#define SCALAR_HH

#include "string.hh"
#include "real.hh"
#include "protected-scm.hh"

#error
/// Perl -like scalar type.
struct Scalar 
{
  Protected_scm scm_;
public:
  Scalar (Real r);
  Scalar (int i);
  Scalar (long l);
  Scalar (char c);
  Scalar (char const *c);
  Scalar (String s);
  Scalar (Rational);
  operator Rational();
  Scalar();
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

