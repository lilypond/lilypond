/*   
  unfolded-repeat-iterator.hh -- declare Unfolded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef UNFOLDED_REPEAT_ITERATOR_HH
#define UNFOLDED_REPEAT_ITERATOR_HH

#include "music-iterator.hh"

/**
   Iterate repeats.  First do body, then alternatives one by one,
   optionally interspersed by the body.
 */
class Unfolded_repeat_iterator : public Music_iterator
{
public:
  /**
     How often have we done the body (assuming bodies are interspersed.)?
   */
  int done_count_;


  /// unfold everything, or do volta?
  bool full_unfold_b_;
  
  /// are we busy doing the body?
  bool do_main_b_;

  /** How far have we progressed into the repeat.
      This excludes the elt currently being iterated.
  */
  Moment done_mom_;
  int alternative_count_i_;
  Music_iterator * current_iter_p_;
  
  /// pointer to the alternative that will be processed next.
  Cons<Music> *alternative_cons_l_;
  ~Unfolded_repeat_iterator();
  Unfolded_repeat_iterator ();


protected:  
  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual void do_process_and_next (Moment);
  virtual Music_iterator *try_music_in_children (Music const*) const;

  virtual bool ok () const;
  virtual void next_element ();
  virtual void do_print () const;
};
#endif /* UNFOLDED_REPEAT_ITERATOR_HH */

