/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/



#include "music-list.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"


IMPLEMENT_IS_TYPE_B1(Sequential_music,Music_sequence);
IMPLEMENT_IS_TYPE_B1(Simultaneous_music,Music_sequence);

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
  return music_p_list_p_->do_relative_octave (p, false);
}

Musical_pitch
Simultaneous_music::to_relative_octave (Musical_pitch p)
{
  return music_p_list_p_->do_relative_octave (p, true);
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


IMPLEMENT_IS_TYPE_B1(Request_chord, Simultaneous_music);


Request_chord::Request_chord()
  : Simultaneous_music (new Music_list)
{
  multi_level_i_ =0;
}


Musical_pitch
Request_chord::to_relative_octave (Musical_pitch last)
{
  for (iter (music_p_list_p_->top(),i); i.ok (); i++)
    {
      Musical_req *m =((Request*)i.ptr ())->access_Musical_req ();
      if (m && m->access_Melodic_req ())
	{	  
	  Musical_pitch &pit = m->access_Melodic_req ()->pitch_;
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
