/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "simultaneous-music-iterator.hh"
#include "music-list.hh"
#include "killing-cons.tcc"

Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
}

Simultaneous_music_iterator::~Simultaneous_music_iterator ()
{
  children_p_list_.junk ();
}

void
Simultaneous_music_iterator::construct_children()
{
  int j = 0;
  Music_sequence const *sim = dynamic_cast<Music_sequence const*> (music_l_);

  for (Cons<Music> *i = sim->music_p_list_p_->head_; i;  i = i->next_, j++)
    {
      Music_iterator * mi = get_iterator_p (i->car_);
      if (mi->ok()) 
	{
#if 0
	  if  (sim->translator_type_str_.empty_b ())
	    set_translator (mi->report_to_l()->ancestor_l (0));	// huh?
#endif

	  children_p_list_.append (new Killing_cons<Music_iterator> (mi,0));
	}
      else
	delete mi;
    }
}


void
Simultaneous_music_iterator::do_print() const
{
#ifndef NPRINT
  for (Cons<Music_iterator> *p = children_p_list_.head_; p; p = p->next_)
    p->car_->print();
#endif
}

void
Simultaneous_music_iterator::do_process_and_next (Moment until)
{
  for (Cons<Music_iterator> **pp = &children_p_list_.head_; *pp; )
    {
      Music_iterator * i = (*pp)->car_;
      if  (i->next_moment() == until) 
	{
	  i->process_and_next (until);
	}
      if (!i->ok())
	delete children_p_list_.remove_cons (pp);
      else
	pp = &(*pp)->next_;
    }
  Music_iterator::do_process_and_next (until);
}




Moment
Simultaneous_music_iterator::next_moment() const
{
  Moment next;
  next.set_infinite (1);
  
  for (Cons<Music_iterator> *p = children_p_list_.head_; p; p = p->next_)
    next = next <? p->car_->next_moment() ;
  return next;
}



bool
Simultaneous_music_iterator::ok() const
{
  return children_p_list_.head_;
}

