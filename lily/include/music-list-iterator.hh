/*
  music-list-iterator.hh -- declare Music_list_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#ifndef MUSIC_LIST_ITERATOR_HH
#define MUSIC_LIST_ITERATOR_HH

#include "music-iterator.hh"
#include "pcursor.hh"
#include "plist.hh"

class Music_list_iterator : public Music_iterator
{
public:
  Music_list_iterator ();
  virtual ~Music_list_iterator ();

  virtual void construct_children ();
  virtual Moment next_moment () const;
  virtual bool ok () const;

protected:
  virtual void do_print () const;
  virtual void do_process_and_next (Moment);
};

#endif // MUSIC_LIST_ITERATOR_HH
