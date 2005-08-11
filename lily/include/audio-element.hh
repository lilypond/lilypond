/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "virtual-methods.hh"

class Audio_element
{
public:
  Audio_element ();
  virtual ~Audio_element ();
  virtual char const *name () const;
protected:
};

#endif // AUDIO_ELEMENT_HH
