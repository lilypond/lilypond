/*
  audio-element.cc -- implement Audio_element

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#include <cassert> 

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
  return this->class_name ();
}
