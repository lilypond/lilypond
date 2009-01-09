/*
  compressed-music-iterator.hh -- declare Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>
*/

#ifndef TIME_SCALED_MUSIC_ITERATOR_HH
#define TIME_SCALED_MUSIC_ITERATOR_HH

#include "sequential-iterator.hh"

class Time_scaled_music_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  DECLARE_CLASSNAME(Time_scaled_music_iterator);
  Time_scaled_music_iterator ();
protected:
  virtual SCM get_music_list () const;
private:
};

#endif /* TIME_SCALED_MUSIC_ITERATOR_HH */
