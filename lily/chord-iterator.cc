/*
  Simultaneous_music-iter.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "chord-iterator.hh"

#include "music-list.hh"


void
Simultaneous_music_iterator::construct_children()
{
  int j = 0;
  for (PCursor<Music*> i (simultaneous_music_l ()->music_p_list_p_->top());  
       i.ok(); j++, i++) 
    {
      Music_iterator * mi = get_iterator_p (i.ptr());
      if (mi->ok()) 
	{
	  if  (simultaneous_music_l ()->translator_type_str_.empty_b ())
	    set_translator (mi->report_to_l()->ancestor_l (simultaneous_music_l ()->multi_level_i_));
	  children_p_list_.bottom().add (mi);
	}
      else 
	delete mi;
    }
}


void
Simultaneous_music_iterator::do_print() const
{
#ifndef NPRINT
  for (PCursor<Music_iterator*> i (children_p_list_.top()); i.ok (); i++) 
    {
      i->print();
    }
#endif
}

void
Simultaneous_music_iterator::do_process_and_next (Moment until)
{
  for (PCursor<Music_iterator*> i (children_p_list_.top()); i.ok ();) 
    {
      if  (i->next_moment() == until) 
	{
	  i->process_and_next (until);
	}
      if (!i->ok()) 
	delete i.remove_p();
      else
	i++;
    }
  Music_iterator::do_process_and_next (until);
}


IMPLEMENT_IS_TYPE_B1(Simultaneous_music_iterator,Music_iterator);

Moment
Simultaneous_music_iterator::next_moment() const
{
  Moment next;
  next.set_infinite (1);
  for (PCursor<Music_iterator*> i (children_p_list_.top()); i.ok (); i++)
    next = next <? i->next_moment() ;
  return next;
}



bool
Simultaneous_music_iterator::ok() const
{
  return children_p_list_.size();
}

Simultaneous_music*
Simultaneous_music_iterator::simultaneous_music_l ()const
{
  return (  Simultaneous_music *) music_l_;
}
