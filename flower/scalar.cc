/*
  scalar.cc -- implement Scalar

  source file of the Flower Library

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <assert.h>
#include <stdio.h>
#include "scalar.hh"
#include "rational.hh"

Scalar::Scalar (Rational r)
{
  (*this) = r.str ();
}

Scalar::operator Rational ()
{
  int p = index_i ('/');
  if (p == -1)
    return int (*this);
  
  String s2 = right_str (length_i ()-p-1);
  String s1 = left_str (p);

  return Rational (s1.value_i (), s2.value_i ());
}

bool
Scalar::isnum_b () const
{
  int conv = false;
  if (length_i ())
    {
      long l =0;
      conv = sscanf (strh_.ch_C (), "%ld", &l);
    }
  return length_i () && conv;
}

Scalar::operator Real()
{
  assert (isnum_b ());
  return value_f ();
}

Scalar::operator int()
{
  if (!length_i ())
    return 0;			// ugh
  
  assert (isnum_b());
  return value_i ();
}

Scalar::operator bool () const
{
  return to_bool ();
}

bool
Scalar::to_bool () const
{
  if (!length_i ())
    return false;
  if (*this == "0")
    return false;
  String u (*this);
  if (u.upper_str () == "FALSE")
    return false;
  return true;
}
