/*
  music-iterator.hh -- declare Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "array.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "interpretation-context-handle.hh"
#include "smobs.hh"

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

  get_pending_events (M) -- return all events starting at M (pre: no events
    before M). No side-effects

  skip (M) -- remove all events starting before M (leave the ones that
    start M).  no side-effects on interpretation context


  TODO:

  merge pending_moment and process?
  
*/
class Music_iterator
{
protected:
  Moment music_length_;
  Moment start_mom_;

  DECLARE_SMOBS (Music_iterator,dummy);
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  
  Moment music_length_mom () const;
  Moment music_start_mom () const;
  Music_iterator ();
  Music_iterator (Music_iterator const&);

  /**
     Do the reporting.  Will try MUSIC_L_ in its own translator first,
     then its children. Returns the iterator that succeeded
  */
  Music_iterator *  try_music (Music  *) const;
  
  /**
    The translation unit that we this iterator is reporting  to now.
   */
  Translator_group* report_to () const;

  void set_translator (Translator_group*);
  
  /** Get an iterator matching the type of MUS, and use TRANS to find
    an accompanying translation unit
   */
  static SCM get_static_get_iterator (Music * mus);
  void init_translator (Music  *, Translator_group *); 

  virtual Moment pending_moment () const;
  virtual bool ok () const;
  virtual SCM get_pending_events (Moment until)const;
  virtual void process (Moment until);
  virtual void skip (Moment until);
  virtual void derived_mark ()const;
  
  /**
    Construct sub-iterators, and set the translator to 
    report to.
   */
  virtual void construct_children ();
  DECLARE_SCHEME_CALLBACK(constructor, ());
  
  /**
    Get an iterator for MUS, inheriting the translation unit from THIS.
   */
  SCM get_iterator (Music *) const;

  virtual Music_iterator* try_music_in_children (Music *) const;

  Music * get_music () const;
private:
  Interpretation_context_handle handle_;
  Music  * music_;
};


#define IMPLEMENT_CTOR_CALLBACK(Class)		\
LY_DEFINE_MEMBER_FUNCTION(Class,constructor, #Class "::constructor",\
	  0,0,0,\
	  (),\
	  "Construct a " #Class " music iterator")\
{						\
  SCM val = (new Class)->self_scm();   \
  scm_gc_unprotect_object (val);\
  return val ;				\
}						\

DECLARE_UNSMOB(Music_iterator, iterator);

#endif // MUSIC_ITERATOR_HH
