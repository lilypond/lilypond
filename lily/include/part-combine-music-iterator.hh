/*   
  part-combine-music-iterator.hh -- declare Part_combine_music_iterator
  
  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#ifndef PART_COMBINE_MUSIC_ITERATOR_HH
#define PART_COMBINE_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

class Part_combine_music_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  Part_combine_music_iterator ();

protected:
  virtual ~Part_combine_music_iterator ();

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void process (Moment);
  virtual SCM get_music (Moment)const;
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual bool ok () const;

private:
  void change_to (Music_iterator*, String, String);

  Music_iterator * first_iter_p_;
  Music_iterator * second_iter_p_;
  Moment first_until_;
  Moment second_until_;
};

#endif /* PART_COMBINE_MUSIC_ITERATOR_HH */

