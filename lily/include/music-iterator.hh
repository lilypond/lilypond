/*
  music-iterator.hh -- declare Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "array.hh"
#include "moment.hh"
#include "virtual-methods.hh"
#include "interpretation-context-handle.hh"
#include "music-iterator-ctor.hh"

/** 
  Conceptually a music-iterator operates on a queue of musical events
  that are pending. This queue does not actually exist, but it is a
  way of viewing and traversing music-expressions.

  
  ok () -- events left ?

  pending_mom () -- time tag of the next event to be processed.
    PRECONDITION: this->ok() holds.
  
  process (M) -- process all at M (Precondition: no events exist
    before M, this->ok() holds).  Side-effects:
    
    * This removes all events at M from the pending queue.

    * Typically this reports the music to an interpretation context,
    thus changing the state of the interpretation context.

  get_music (M) -- return all events starting at M (pre: no events
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

public:
  VIRTUAL_COPY_CONS (Music_iterator);

  Moment music_length_mom () const;
  Music_iterator ();
  Music_iterator (Music_iterator const&);
  virtual ~Music_iterator ();

  /**
     Do the reporting.  Will try MUSIC_L_ in its own translator first,
     then its children. Returns the iterator that succeeded
  */
  Music_iterator *  try_music (Music  *) const;
  
  /**
    The translation unit that we this iterator is reporting  to now.
   */
  Translator_group* report_to_l () const;

  void set_translator (Translator_group*);
  
  /** Get an iterator matching the type of MUS, and use TRANS to find
    an accompanying translation unit
   */
  static Music_iterator* static_get_iterator_p (Music * mus);
  void init_translator (Music  *, Translator_group *); 

  virtual Moment pending_moment () const;
  virtual bool ok () const;
  virtual SCM get_music (Moment until)const;
  virtual void process (Moment until);
  virtual void skip (Moment until);

  /**
    Construct sub-iterators, and set the translator to 
    report to.
   */
  virtual void construct_children ();
  static SCM constructor_cxx_function;
  
protected:
  Music  * music_l_;

  /**
    Get an iterator for MUS, inheriting the translation unit from THIS.
   */
  Music_iterator* get_iterator_p (Music *) const;

  virtual Music_iterator* try_music_in_children (Music *) const;

private:
  Interpretation_context_handle handle_;
};


/*
  implement Class::constructor, a SCM function that
  returns an encapsulated factory function.
 */
#define IMPLEMENT_CTOR_CALLBACK(Class)		\
static void *						\
Class ## _ctor (SCM)				\
{						\
  return new Class ;				\
}						\
SCM Class :: constructor_cxx_function;\
void						\
Class ## _constructor_init()				\
{						\
  SCM s = smobify_cxx_function (& Class ## _ctor);	\
  scm_permanent_object (s);\
  gh_define (#Class "::constructor", s);\
  Class :: constructor_cxx_function = s;\
}\
ADD_SCM_INIT_FUNC(Class ## _ctor_init, Class ## _constructor_init); 

 





#endif // MUSIC_ITERATOR_HH
