/*
  change-iterator.hh -- declare Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CHANGE_ITERATOR_HH
#define CHANGE_ITERATOR_HH

#include "music-iterator.hh"


class Change_iterator : public Music_iterator
{
  void  error (String);

protected:
  virtual void do_process_and_next (Moment);

public:
  /*
    CTOR is public
   */
};

#endif
