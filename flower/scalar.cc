#if 0
/*
  scalar.cc -- implement Scalar

  source file of the Flower Library

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <assert.h>
#include <stdio.h>

#include "rational.hh"

Scalar::Scalar (Rational r)
{
  (*this) = r.str ();
}

Scalar::operator Rational ()
{
  return to_rat ();
}

Rational
Scalar::to_rat () const
{
  int p = index_i ('/');
  if (p == -1)
    return this->to_i ();
  
  String s2 = right_str (length_i ()-p-1);
  String s1 = left_str (p);

  return Rational (s1.value_i (), s2.value_i ());
}

bool
Scalar::isdir_b () const
{
  int conv = length_i ();
  if (conv)
    {
      long l =0;
      conv = sscanf (strh_.ch_C (), "%ld", &l);
      conv = conv && (l >= -1 && l <= 1);
    }
  return conv;
}

bool
Scalar::isnum_b () const
{
  int conv = false;
  if (length_i ())
    {
      long l =0;
      conv = sscanf (strh_.ch_C (), "%lf", &l);
    }
  return length_i () && conv;
}

Scalar::operator Real()
{
  return to_f ();
}

Real
Scalar::to_f () const
{
  assert (isnum_b ());
  return value_f ();
}

Scalar::operator int ()
{
  return to_i ();
}

int
Scalar::to_i () const
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


#endif
