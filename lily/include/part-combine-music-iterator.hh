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

  enum State { UNKNOWN, UNRELATED=1, SOLO1=2, SOLO2=4, UNIRHYTHM=8, UNISON=16, UNISILENCE=32, SPLIT_INTERVAL=64 };
  static SCM constructor_cxx_function; 
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
  int get_state (Moment m);

  Music_iterator * first_iter_p_;
  Music_iterator * second_iter_p_;
  Moment first_until_;
  Moment second_until_;
  int state_;
  String suffix_;
};

#endif /* PART_COMBINE_MUSIC_ITERATOR_HH */

