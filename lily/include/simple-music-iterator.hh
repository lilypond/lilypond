/*   
  simple-music-iterator.hh -- declare Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  Moment last_processed_mom_;
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  DECLARE_SCHEME_CALLBACK(constructor, ()); 
  Simple_music_iterator ();
  Simple_music_iterator (Simple_music_iterator const &);
  virtual void process (Moment);
  virtual bool ok ()const;
  virtual Moment pending_moment ()const;

  virtual void skip (Moment);
};

#endif /* SIMPLE_MUSIC_ITERATOR_HH */

