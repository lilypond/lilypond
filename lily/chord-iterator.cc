/*
  chord-iter.cc -- implement Chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "translator-group.hh"
#include "debug.hh"
#include "chord-iterator.hh"

#include "music-list.hh"


Chord_iterator::~Chord_iterator()
{
}

Chord_iterator::Chord_iterator (Chord const *chord_C)
{
  chord_C_ = chord_C;
}

void
Chord_iterator::construct_children()
{
  int j = 0;
  for (PCursor<Music*> i (chord_C_->music_p_list_.top());  
       i.ok(); j++, i++) 
    {
      Music_iterator * mi = get_iterator_p (i.ptr());
      if (mi->ok()) 
	{
	  if  (chord_C_->translator_type_str_.empty_b ())
	    set_translator (mi->report_to_l()->ancestor_l (chord_C_->multi_level_i_));
	  children_p_list_.bottom().add (mi);
	}
      else 
	delete mi;
    }
}
void
Chord_iterator::do_print() const
{
#ifndef NPRINT
  for (PCursor<Music_iterator*> i (children_p_list_.top()); i.ok (); i++) 
    {
      i->print();
    }
#endif
}

void
Chord_iterator::process_and_next (Moment until)
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
  Music_iterator::process_and_next (until);
}


IMPLEMENT_IS_TYPE_B1(Chord_iterator,Music_iterator);

Moment
Chord_iterator::next_moment() const
{
  Moment next;
  next.set_infinite (1);
  for (PCursor<Music_iterator*> i (children_p_list_.top()); i.ok (); i++)
    next = next <? i->next_moment() ;
  return next;
}



bool
Chord_iterator::ok() const
{
  return children_p_list_.size();
}
