/*
  music-iterator.hh -- declare Music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "array.hh"
#include "moment.hh"
#include "virtual-methods.hh"

/** Walk through music and deliver music to translation units, synced
  in time.  This classa provides both the implementation of the shared
  code, and the public interface.

  Derived classes should only have a public constructor.
  The state of an iterator would be the intersection of the particular music 
  construct with one point in musical time.
 */
class Music_iterator {
  Array<Translator_group*>report_to_l_arr_;
  void push_translator (Translator_group*);
  void pop_translator();

protected:
  Music const * music_l_;
  bool first_b_;

  /**
    Do the actual printing.  This should be overriden in derived classes.  It 
    is called by #print#, in the public interface
   */
  virtual void do_print() const;
    
  /**
    Find a bottom notation context to deliver requests to.
   */
  virtual Translator_group* get_req_translator_l();

  /**
    Get an iterator for MUS, inheriting the translation unit from THIS.
   */
  Music_iterator* get_iterator_p (Music const*mus) const;
  void set_translator (Translator_group*);

  /** Do the actual reporting.  This should be overriden in derived
    classes.  It is called by #process_and_next#, the public interface 
    */
  virtual void do_process_and_next (Moment until);

public:

  /**
    The translation unit that we this iterator is reporting  to now.
   */
  Translator_group*report_to_l() const;

  
  /** Get an iterator matching the type of MUS, and use TRANS to find
    an accompanying translation unit
   */
  static Music_iterator* static_get_iterator_p (Music const* mus,Translator_group* trans);

  Music_iterator();
    
  ///  Find the next interesting point in time.
  virtual Moment next_moment() const;

  ///Are we finished with this piece of music?
  virtual bool ok() const;

  virtual ~Music_iterator();


  ///Report all musical information that occurs between now and UNTIL
  void process_and_next (Moment until);

  /**
    Construct sub-iterators, and set the translator to 
    report to.
   */
  virtual void construct_children();
  void print() const;
};

#endif // MUSIC_ITERATOR_HH
