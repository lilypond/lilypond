/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "simultaneous-music-iterator.hh"

#include "music-list.hh"

Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
}

Simultaneous_music_iterator::~Simultaneous_music_iterator ()
{
}

void
Simultaneous_music_iterator::construct_children()
{
  int j = 0;
  Simultaneous_music const *sim = dynamic_cast<Simultaneous_music const*> (music_l_);
  for (PCursor<Music*> i (sim->music_p_list_p_->top());  
       i.ok(); j++, i++) 
    {
      Music_iterator * mi = get_iterator_p (i.ptr());
      if (mi->ok()) 
	{
	  if  (sim->translator_type_str_.empty_b ())
	    set_translator (mi->report_to_l()->ancestor_l (0));	// huh?
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

