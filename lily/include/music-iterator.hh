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

class Music_iterator {
  Array<Translator_group*>report_to_l_arr_;
  void push_translator (Translator_group*);
  void pop_translator();
protected:
  Music const * music_l_;
  bool first_b_;
  virtual void do_print() const;
    
  virtual Translator_group* get_req_translator_l();
  Music_iterator* get_iterator_p (Music const*) const;
  void set_translator (Translator_group*);
  virtual void do_process_and_next (Moment until);
  void inherit_info (Music_iterator const *parent);
public:
  Translator_group*report_to_l() const;
  DECLARE_MY_RUNTIME_TYPEINFO;
 
  static Music_iterator* static_get_iterator_p (Music const*,Translator_group*);
  Music_iterator();
    
  virtual Moment next_moment() const;
  virtual bool ok() const;
  virtual ~Music_iterator();
  void process_and_next (Moment until);

  /**
    Construct sub-iterators, and set the translator to 
    report to
   */
  virtual void construct_children();
  void print() const;
};

#endif // MUSIC_ITERATOR_HH
