/*
  compressed-music-iterator.hh -- declare Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef Time_scaled_music_ITERATOR_HH
#define Time_scaled_music_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Time_scaled_music_iterator : public Music_wrapper_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
protected:
  virtual void process (Moment);
};

#endif /* Time_scaled_music_ITERATOR_HH */

