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
  /* constructor is public */
  virtual void do_process (Moment);

private:
  void  error (String);
};

#endif
