/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

#include "virtual-methods.hh"

struct Audio_element {
  void print ()const;
  
  virtual ~Audio_element();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_print () const;
};
#endif // AUDIO_ELEMENT_HH
