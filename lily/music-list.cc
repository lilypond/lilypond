/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"
#include "main.hh"
#include "killing-cons.tcc"

Moment
Simultaneous_music::length_mom () const
{
  Moment dur = 0;
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    dur = dur >? i->car_->length_mom ();

  return dur;
}

void
Music_sequence::compress (Moment m)
{
  
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    i->car_->compress (m);
}

Simultaneous_music::Simultaneous_music(Music_list *p)
  : Music_sequence (p)
{

}

Sequential_music::Sequential_music(Music_list *p)
  : Music_sequence (p)
{
}

Moment
Sequential_music::length_mom () const
{
  Moment last=0;
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    {
      last += i->car_->length_mom ();
    }
  return  last;
}

Musical_pitch
Sequential_music::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, false);
}

Musical_pitch
Simultaneous_music::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, true);
}

Musical_pitch
Music_sequence::do_relative_octave (Musical_pitch p, bool b)
{
  return music_p_list_p_->do_relative_octave (p, b);  
}

Musical_pitch 
Music_list::do_relative_octave (Musical_pitch last, bool ret_first)
{
  Musical_pitch retval;
  int count=0;
  for (Cons<Music> *i = head_; i ; i = i->next_)
    {
      last = i->car_->to_relative_octave (last);
      if (!count ++ )
	retval = last;
    }

  if (!ret_first)
    retval = last;
  
  return retval;
}


Music_list::Music_list (Music_list const &s)
  : Cons_list<Music> (s)
{
  init_list ();
  clone_killing_cons_list (*this, s.head_);
}


void
Music_list::add_music (Music*m_p)
{
  if (!m_p)
    return;

  append (new Killing_cons<Music> (m_p, 0));
}

Request_chord::Request_chord()
  : Simultaneous_music (new Music_list)
{
}


Musical_pitch
Request_chord::to_relative_octave (Musical_pitch last)
{
  for (Cons<Music> *i = music_p_list_p_->head_; i ; i = i->next_)
    {
      if (Melodic_req *m= dynamic_cast <Melodic_req *> (i->car_))
	{
	  Musical_pitch &pit = m->pitch_;
	  pit.to_relative_octave (last);
	  return pit;
	}
    }
  return last;
}


Music_list::Music_list ()
{
}
