/*
  audio-element.cc -- implement Audio_element

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "audio-element.hh"

Audio_element::Audio_element ()
{
}

Audio_element::~Audio_element ()
{
}

char const * 
Audio_element::name () const
{
  return classname (this);
}
