/*
  music-list.cc -- implement Music_list, 

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#include "music.hh"
#include "debug.hh"

Music_list::Music_list(Music_list const&s)
{
    for (iter(s.music_p_list_.top(), i); i.ok(); i++)
	add(i->clone());
}


IMPLEMENT_STATIC_NAME(Music_list);

void
Music_list::add(Music*mus_p)
{
    music_p_list_.bottom().add(mus_p);
}

void
Music_list::transpose(Melodic_req const*m)
{
  for (iter(music_p_list_.top(), i); i.ok(); i++)
      i->transpose(m);
}

void
Music_list::do_print() const
{
    for (iter(music_p_list_.top(), i); i.ok(); i++)
	i->print();
}

IMPLEMENT_STATIC_NAME(Chord);

void
Chord::translate(Moment dt)
{
    for (iter(music_p_list_.top(), i); i.ok(); i++)
	i->translate(dt);
}

MInterval
Chord::time_int()const
{
    MInterval m;
    for (iter(music_p_list_.top(), i); i.ok(); i++)
	m.unite(i->time_int());
    return m;
}

MInterval
MVoice::time_int() const
{
    Moment last=0;
    for (iter(music_p_list_.top(), i); i.ok(); i++)
	last += i->time_int().length();
    return MInterval (0,last);
}

void
MVoice::translate(Moment dt)
{
    for (iter(music_p_list_.top(), i); i.ok(); i++)
	i->translate(dt);
}

IMPLEMENT_STATIC_NAME(MVoice);
