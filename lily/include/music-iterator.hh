/*
  music-iterator.hh -- declare Music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef MUSIC_ITERATOR_HH
#define MUSIC_ITERATOR_HH

#include "lily-proto.hh"
#include "plist.hh"
#include "varray.hh"
#include "moment.hh"
#include "virtual-methods.hh"

class Music_iterator {
  Array<Translator *>report_to_l_arr_;
  void push_translator (Translator*);
  void pop_translator();
protected:
  bool first_b_;
  virtual void do_print() const;
    
  virtual Translator * get_req_translator_l();
  Music_iterator* get_iterator_p (Music*) const;
  void set_translator (Translator*);
  Music_iterator *daddy_iter_l_;
    
public:
  Translator *report_to_l() const;
  DECLARE_MY_RUNTIME_TYPEINFO;
 
  static Music_iterator* static_get_iterator_p (Music*,Translator*);
  Music_iterator();
    
  virtual void process_and_next (Moment until);
  virtual Moment next_moment() const;
  virtual bool ok() const;
  virtual ~Music_iterator();
  virtual void construct_children();
  void print() const;
};

#endif // MUSIC_ITERATOR_HH
