/*   
  unfolded-repeat-iterator.hh -- declare Unfolded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  void add_repeat_command (SCM);

public:
  VIRTUAL_COPY_CONS (Music_iterator);
  /**
     How often have we done the body (assuming bodies are interspersed.)?
   */
  int done_count_;
  static SCM constructor_cxx_function; 

  /*
    are we now busy doing the body?

   */
  bool do_main_b_;

  /*
    are we doing volta's?
   */
  bool volta_b_;

  /** How far have we progressed into the repeat.
      This excludes the elt currently being iterated.
  */
  Moment here_mom_;
  int alternative_count_i_;
  Music_iterator * current_iter_p_;
  
  /// pointer to the alternative that will be processed next.
  SCM alternative_cons_;
  ~Unfolded_repeat_iterator();
  Unfolded_repeat_iterator ();

protected:  
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void process (Moment);
  virtual Music_iterator *try_music_in_children (Music *) const;
  virtual void skip (Moment);
  virtual SCM get_music (Moment) const;
  
  virtual bool ok () const;
  virtual void next_element (bool side_effect);
};

class Volta_repeat_iterator : public Unfolded_repeat_iterator
{
public:
  Volta_repeat_iterator ();
  static  SCM constructor_cxx_function;
  VIRTUAL_COPY_CONS(Music_iterator);
};


#endif /* UNFOLDED_REPEAT_ITERATOR_HH */

