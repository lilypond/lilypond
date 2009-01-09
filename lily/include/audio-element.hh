/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "virtual-methods.hh"

class Audio_element
{
public:
  Audio_element ();
  virtual ~Audio_element ();

  DECLARE_CLASSNAME(Audio_element);
  virtual char const *name () const;
};

#endif // AUDIO_ELEMENT_HH
