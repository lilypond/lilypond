/*
  audio-element.cc -- implement Audio_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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


