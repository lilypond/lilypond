/*
  Music_list-iterator.cc -- implement Music_list_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "music-list-iterator.hh"
#include "music-list.hh"

Music_list_iterator::Music_list_iterator ()
{
}

Music_list_iterator::~Music_list_iterator ()
{
}

void
Music_list_iterator::construct_children ()
{
}

void
Music_list_iterator::do_print() const
{
}

void
Music_list_iterator::do_process_and_next (Moment)
{
}

Moment
Music_list_iterator::next_moment () const
{
  return 0;
}

bool
Music_list_iterator::ok () const
{
  return false;
}

