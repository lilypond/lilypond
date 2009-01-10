/*
  sequential-iterator.hh -- declare Sequential_iterator

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SEQUENTIAL_ITERATOR_HH
#define SEQUENTIAL_ITERATOR_HH

#include "music-iterator.hh"
#include "protected-scm.hh"

/** Sequential_music iteration: walk each element in turn, and
    construct an iterator for every element.
*/
class Sequential_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Sequential_iterator);
  Sequential_iterator ();
  Sequential_iterator (Sequential_iterator const &);
  virtual void derived_substitute (Context *f, Context *t);

  virtual void derived_mark () const;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;

protected:
  virtual void process (Moment);
  virtual bool run_always () const;
  
protected:
  Music_iterator *iter_;
  
  virtual SCM get_music_list () const;
  virtual void next_element (bool side_effect);

  Grace_fixup *get_grace_fixup () const;
  void next_grace_fixup ();

private:
  Moment last_mom_;
  Moment here_mom_;
  SCM cursor_;
  Grace_fixup *grace_fixups_;
};

#endif /* SEQUENTIAL_ITERATOR_HH */
