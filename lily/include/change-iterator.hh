/*
  change-iterator.hh -- declare Change_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CHANGE_ITERATOR_HH
#define CHANGE_ITERATOR_HH

#include "music-iterator.hh"


class Change_iterator : public Music_iterator
{
public:
  VIRTUAL_COPY_CONS (Music_iterator);
  /*
    CTOR is public
   */

protected:
  virtual void do_process_and_next (Moment);

private:
  void  error (String);
};

#endif
