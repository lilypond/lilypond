
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
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Lyric_combine_music_iterator ();
  static SCM constructor_cxx_function;
protected:
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;
  virtual ~Lyric_combine_music_iterator ();

private:
  Music_iterator * music_iter_p_;
  Music_iterator * lyric_iter_p_;
};
#endif /* LYRIC_COMBINE_MUSIC_ITERATOR_HH */

