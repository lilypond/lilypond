/*
  music-iterator.hh -- declare Music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "std-vector.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "context-handle.hh"

/**
   ---

   Music_iterator is an object type that traverses the Music structure and
   reports the events it finds to interpretation contexts. It is not yet
   user-serviceable.


   ---

   Conceptually a music-iterator operates on a queue of musical events
   that are pending. This queue does not actually exist, but it is a
   way of viewing and traversing music-expressions.


   ok () -- events left ?

   pending_mom () -- time tag of the next event to be processed.
   PRECONDITION: this->ok () holds.

   process (M) -- process all at M (Precondition: no events exist
   before M, this->ok () holds).  Side-effects:

   * This removes all events at M from the pending queue.

   * Typically this reports the music to an interpretation context,
   thus changing the state of the interpretation context.


   TODO:

   merge pending_moment and process?
*/
class Music_iterator
{
protected:
  Moment music_length_;
  Moment start_mom_;

  DECLARE_CLASSNAME(Music_iterator);
  DECLARE_SMOBS (Music_iterator);
  Music_iterator (Music_iterator const &);

public:
  Moment music_get_length () const;
  Moment music_start_mom () const;
  Music_iterator ();
  void report_event (Music *);
  Context *get_outlet () const;
  void set_context (Context *);
  static SCM get_static_get_iterator (Music *mus);
  void init_context (Music *, Context *);
  void quit ();
  void substitute_outlet (Context *from, Context *to);
  void descend_to_bottom_context ();
  virtual void derived_substitute (Context *, Context *);
  virtual Moment pending_moment () const;
  virtual bool ok () const;
  virtual bool run_always () const;
  virtual void process (Moment until);
  virtual void derived_mark () const;
  virtual void construct_children ();
  DECLARE_SCHEME_CALLBACK (constructor, ());
  SCM get_iterator (Music *) const;

  Music *get_music () const;
protected:
  virtual void do_quit ();
  void descend_to_child (Context *);

private:
  Context_handle handle_;
  Music *music_;
};

bool is_child_context (Context *me, Context *child);

#define IMPLEMENT_CTOR_CALLBACK(Class)					\
  LY_DEFINE_MEMBER_FUNCTION (Class, constructor, \
			     mangle_cxx_identifier (string (#Class) + "::constructor").c_str(), \
			     0, 0, 0,					\
			     (),					\
			     "")					\
  {									\
    Class *c = (new Class);						\
    return c->unprotect ();						\
  }

DECLARE_UNSMOB (Music_iterator, iterator);

#endif // MUSIC_ITERATOR_HH
