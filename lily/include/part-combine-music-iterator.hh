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
  Part_combine_music_iterator ();

protected:
  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual void do_process_and_next (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;

  virtual bool ok () const;
  virtual void do_print () const;
  virtual ~Part_combine_music_iterator ();

private:
  void change_to (Music_iterator*, String, String);

  Music_iterator * first_iter_p_;
  Music_iterator * second_iter_p_;
  Moment now_;
  Moment first_until_;
  Moment second_until_;

  bool combined_b_;
};

#endif /* PART_COMBINE_MUSIC_ITERATOR_HH */

