/*
  simple-music-iterator.hh -- declare Simple_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SIMPLE_MUSIC_ITERATOR_HH
#define SIMPLE_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

/*
  Iterator for atomic music objects: events are generated at the
  beginning and at the end of the music.
*/
class Simple_music_iterator : public Music_iterator
{
protected:
  DECLARE_CLASSNAME(Simple_music_iterator);

  Moment last_processed_mom_;
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Simple_music_iterator ();
  virtual void process (Moment);
  virtual bool ok ()const;
  virtual Moment pending_moment ()const;
};

#endif /* SIMPLE_MUSIC_ITERATOR_HH */

