
/*   
  lyric-combine-music-iterator.hh -- declare Lyric_combine_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef LYRIC_COMBINE_MUSIC_ITERATOR_HH
#define LYRIC_COMBINE_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

class Lyric_combine_music_iterator : public Music_iterator
{
  Music_iterator * music_iter_p_;
  Music_iterator * lyric_iter_p_;
  
protected:
  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual void do_process_and_next (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;
  virtual void do_print () const;
  virtual ~Lyric_combine_music_iterator ();
public:
  Lyric_combine_music_iterator ();
};
#endif /* LYRIC_COMBINE_MUSIC_ITERATOR_HH */

