/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "virtual-methods.hh"

struct Audio_element {
  void print ()const;
  
  virtual ~Audio_element();
  
protected:
  virtual void do_print () const;
};
#endif // AUDIO_ELEMENT_HH
