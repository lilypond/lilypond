/*
  Simultaneous_music-iterator.cc -- implement Simultaneous_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

Simultaneous_music_iterator::Simultaneous_music_iterator (Simultaneous_music_iterator const& src)
  : Music_iterator (src)
{
  separate_contexts_b_ = src.separate_contexts_b_;
  for (Cons<Music_iterator> *p = src.children_p_list_.head_; p; p = p->next_)
    {
      Music_iterator *i = p->car_;
      children_p_list_.append (new Killing_cons<Music_iterator> (i->clone (), 0));
    }
}

Simultaneous_music_iterator::~Simultaneous_music_iterator ()
{
  children_p_list_.junk ();
}

SCM
Simultaneous_music_iterator::get_music (Moment m)const
{
  SCM s = SCM_EOL;
  for (Cons<Music_iterator> *p = children_p_list_.head_; p; p = p->next_)
    {
      s = gh_append2 (p->car_->get_music (m), s);
    }
  return s;
}

void
Simultaneous_music_iterator::construct_children ()
{
  int j = 0;
  Music_sequence const *sim = dynamic_cast<Music_sequence const*> (music_l_);

  SCM i = sim->music_list ();
  for (; gh_pair_p (i); i = gh_cdr (i), j++)
    {
      Music *mus = unsmob_music (gh_car (i));
      Music_iterator * mi = static_get_iterator_p (mus);

      /* if separate_contexts_b_ is set, create a new context with the
	 number number as name */
      
      Translator_group * t = (j && separate_contexts_b_)
	? report_to_l ()->find_create_translator_l (report_to_l ()->type_str_,
						    to_str (j))
	: report_to_l ();

      if (!t)
	t = report_to_l ();

      mi->init_translator (mus, t);
      mi->construct_children ();
      
      if (mi->ok ()) 
	{
	  children_p_list_.append (new Killing_cons<Music_iterator> (mi,0));
	}
      else
	delete mi;
    }
}


void
Simultaneous_music_iterator::process (Moment until)
{
  for (Cons<Music_iterator> **pp = &children_p_list_.head_; *pp;)
    {
      Music_iterator * i = (*pp)->car_;
      if (i->pending_moment () == until) 
	{
	  i->process (until);
	}
      if (!i->ok ())
	delete children_p_list_.remove_cons (pp);
      else
	pp = & (*pp)->next_;
    }
}

void
Simultaneous_music_iterator::skip (Moment until)
{
  for (Cons<Music_iterator> **pp = &children_p_list_.head_; *pp;)
    {
      Music_iterator * i = (*pp)->car_;
      if (i->pending_moment () <= until) 
	{
	  i->skip (until);
	}
      if (!i->ok ())
	delete children_p_list_.remove_cons (pp);
      else
	pp = & (*pp)->next_;
    }
}

Moment
Simultaneous_music_iterator::pending_moment () const
{
  Moment next;
  next.set_infinite (1);
  
  for (Cons<Music_iterator> *p = children_p_list_.head_; p; p = p->next_)
    next = next <? p->car_->pending_moment () ;
  return next;
}



bool
Simultaneous_music_iterator::ok () const
{
  return children_p_list_.head_;
}

Music_iterator*
Simultaneous_music_iterator::try_music_in_children (Music *m) const
{
  Music_iterator * b=0;
  for (Cons<Music_iterator> *p = children_p_list_.head_; !b && p; p = p->next_)
    b =p->car_->try_music (m);
  return b;
}



IMPLEMENT_CTOR_CALLBACK (Simultaneous_music_iterator);
