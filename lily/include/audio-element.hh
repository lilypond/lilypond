/*
  audio-element.hh -- declare Audio_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef AUDIO_ELEMENT_HH
#define AUDIO_ELEMENT_HH

 #include <typeinfo>
#include "virtual-methods.hh"

class Audio_element
{
public:
  Audio_element ();
  virtual ~Audio_element ();



  // should we use a scm list?
  bool grace_b_;
  
protected:
};

#endif // AUDIO_ELEMENT_HH
