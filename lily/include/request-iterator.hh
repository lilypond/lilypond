/*
  request-iter.hh -- declare Request_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REQUEST_ITER_HH
#define REQUEST_ITER_HH

#include "music-iterator.hh"

class Request_chord_iterator : public Music_iterator {
  const Request_chord * elt_l_;
  Moment elt_duration_;
  bool last_b_;
public:
  Request_chord_iterator (Request_chord*);
  DECLARE_MY_RUNTIME_TYPEINFO;

protected:
  virtual void process_and_next (Moment);
  virtual Moment next_moment() const;
  virtual void construct_children();
  virtual bool ok() const;
  virtual void do_print() const;
};


#endif // REQUEST_ITER_HH
