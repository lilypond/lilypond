/*
  translation-property.cc -- implement Translation_property

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translation-property.hh"
#include "debug.hh"



void
Translation_property::do_print () const
{
#ifndef NPRINT
  DOUT << "." << var_str_ << " = " << value_;
#endif
}
