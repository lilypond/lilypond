/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/



#include "music-list.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"





Moment
Simultaneous_music::duration () const
{
  Moment dur = 0;
  for (iter (music_p_list_p_->top(), i); i.ok (); i++)
    dur = dur >? i->duration ();

  return dur;
}

void
Music_sequence::compress (Moment m)
{
  for (PCursor<Music*>  i(music_p_list_p_->top()); i.ok (); i++)
    i->compress (m);
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
Sequential_music::duration () const
{
  Moment last=0;
  for (iter (music_p_list_p_->top(), i); i.ok (); i++) 
    {
      last += i->duration ();
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
  for (PCursor<Music*> i (top ()); i.ok (); i++)
    {
      last = i->to_relative_octave (last);
      if (!count ++ )
	retval = last;
    }
  if (!ret_first)
    retval = last;
  return retval;
}


Music_list::Music_list (Music_list const &s)
  : Pointer_list<Music*> ()
{
  for (PCursor<Music*> i(s.top()); i.ok (); i++)
    add_music (i->clone());
}

void
Music_list::add_music (Music*m_p)
{
  if (!m_p)
    return;

  bottom().add (m_p);
}





Request_chord::Request_chord()
  : Simultaneous_music (new Music_list)
{
}


Musical_pitch
Request_chord::to_relative_octave (Musical_pitch last)
{
  for (iter (music_p_list_p_->top(),i); i.ok (); i++)
    {
      if (Melodic_req *m= dynamic_cast <Melodic_req *> (i.ptr ()))
	{
	  Musical_pitch &pit = m->pitch_;
	  pit.to_relative_octave (last);
	  return pit;
	}
    }
  return last;
}


Music_list::Music_list ()
  : Pointer_list<Music*> ()
{
}
