/*
  translation-property.cc -- implement Translation_property

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "translation-property.hh"
#include "debug.hh"

IMPLEMENT_IS_TYPE_B1(Translation_property, Music);

void
Translation_property::do_print () const
{
#ifndef NPRINT
  DOUT << "." << var_str_ << " = " << String(value_);
#endif
}
