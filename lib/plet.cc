/*
  plet.cc -- implement Plet

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "plet.hh"


Plet::Plet ()
{
  type_i_ = 1;
  iso_i_ = 1;
}

Moment
Plet::mom () const
{
  return  Moment (iso_i_, type_i_);
}

bool
Plet::unit_b () const
{
  return type_i_ == 1 && iso_i_ == 1;
}

