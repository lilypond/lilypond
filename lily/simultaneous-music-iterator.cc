/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "simultaneous-music-iterator.hh"
#include "music-list.hh"
#include "killing-cons.tcc"

Simultaneous_music_iterator::Simultaneous_music_iterator ()
{
  separate_contexts_b_ = false;
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

  Cons<Music> *i = (sim->music_p_list_p_) ? sim->music_p_list_p_->head_ : 0;
  for (; i;  i = i->next_, j++)
    {
      Music_iterator * mi = static_get_iterator_p (i->car_);

      /* if separate_contexts_b_ is set, create a new context with the
	 number number as name */
      
      Translator_group * t = (j && separate_contexts_b_)
	? report_to_l ()->find_create_translator_l (report_to_l()->type_str_,
						    to_str (j))
	: report_to_l ();

      if (!t)
	t = report_to_l ();

      mi->init_translator (i->car_, t);
      mi->construct_children ();
      
      if (mi->ok()) 
	{
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

Music_iterator*
Simultaneous_music_iterator::try_music_in_children (Music const*m) const
{
  Music_iterator * b=0;
  for (Cons<Music_iterator> *p = children_p_list_.head_; !b && p; p = p->next_)
    b =p->car_->try_music (m);
  return b;
}
