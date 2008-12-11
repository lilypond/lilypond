/*
  music-wrapper-iterator.hh -- declare Music_wrapper_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_WRAPPER_ITERATOR_HH
#define MUSIC_WRAPPER_ITERATOR_HH

#include "music-iterator.hh"

/**
   The iterator for a #Music_wrapper#.  Since #Music_wrapper# essentially
   does nothing, this iterator creates a child iterator and delegates
   all work to that child.
*/
class Music_wrapper_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Music_wrapper_iterator ();
  DECLARE_CLASSNAME(Music_wrapper_iterator);

  virtual void derived_substitute (Context *f, Context *t);
  virtual void derived_mark () const;
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;
  virtual bool run_always () const;
protected:
  virtual void process (Moment);

  Music_iterator *child_iter_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */

