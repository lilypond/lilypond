#if 0
/*   
  new-repeated-music-iterator.cc --  implement New_repeated_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "music-iterator.hh"


/**
   
 */
class New_repeated_music_iterator : public Music_iterator
{
  Music_iterator * main_iter_p_;
  Music_iterator * alternative_iter_p_;
  int count_;
  
public:
  New_repeated_music_iterator ();
  ~New_repeated_music_iterator ();
  
  
  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual void do_print () const;
  virtual void do_process_and_next (Moment);
};

New_repeated_music_iterator::New_repeated_music_iterator ()
{
  
}
#endif
