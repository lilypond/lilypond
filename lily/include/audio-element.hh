/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "virtual-methods.hh"

struct Audio_element {
    virtual ~Audio_element();
    DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // AUDIO_ELEMENT_HH
