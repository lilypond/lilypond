/*
  plet.cc -- implement Plet

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "plet.hh"


Plet::Plet ()
{
  type_i_ = 1;
  iso_i_ = 1;
}

Rational
Plet::mom () const
{
  return  Rational (iso_i_, type_i_);
}

bool
Plet::unit_b () const
{
  return type_i_ == 1 && iso_i_ == 1;
}

