/*
  audio-element.cc -- implement Audio_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "audio-element.hh"
#include "debug.hh"

Audio_element::Audio_element ()
{
  grace_b_ = false;
}

Audio_element::~Audio_element()
{
}

void
Audio_element::print () const
{
#ifndef NPRINT
  DOUT << classname (this) << "{ ";
  do_print ();
  DOUT << "}";
#endif
}

void
Audio_element::do_print ()const
{
}
