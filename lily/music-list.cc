/*
  music-list.cc -- implement Music_list, Chord, Voice

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <limits.h>
#include "music.hh"
#include "debug.hh"
#include "music-list.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"

Music_list::Music_list (Music_list const&s)
  : Music (s)
{
  multi_level_i_ = s.multi_level_i_;   
  for (iter (s.music_p_list_.top(), i); i.ok (); i++)
    add (i->clone());
}

IMPLEMENT_IS_TYPE_B1(Music_list, Music);
IMPLEMENT_IS_TYPE_B1(Voice,Music_list);
IMPLEMENT_IS_TYPE_B1(Chord,Music_list);

MInterval
Chord::time_int() const
{
  MInterval m;
  for (iter (music_p_list_.top(), i); i.ok (); i++)
    m.unite (i->time_int());

  return m;
}

void
Chord::translate (Moment m)
{
  for (iter (music_p_list_.top(), i); i.ok (); i++)
    i->translate (m); 
}

Chord::Chord()
{

}

Voice::Voice()
{
  offset_mom_ =0;
}

MInterval
Voice::time_int() const
{
  Moment last=0;
  for (iter (music_p_list_.top(), i); i.ok (); i++) 
    {
      MInterval interval = i->time_int();
	
      /*
	c4 <> c4
      */
      if (!interval.empty_b())
	last += interval.length();
    }
  return  offset_mom_ + MInterval (0,last);
}

Musical_pitch
Voice::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, false);
}

Musical_pitch
Chord::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, true);
}

void
Voice::translate (Moment dt)
{
  offset_mom_ += dt;
}


Music_list::Music_list()
{
  multi_level_i_ = 0;
}

void
Music_list::add (Music*m_p)
{
  if (!m_p)
    return;

  m_p->parent_music_l_ = this;
  music_p_list_.bottom().add (m_p);
}

void
Music_list::transpose (Musical_pitch rq)
{
  for (iter (music_p_list_.top(),i); i.ok (); i++)
    i->transpose (rq);    
}

void
Music_list::do_print() const
{
#ifndef NPRINT
  for (iter (music_p_list_.top(),i); i.ok (); i++)
    i->print();
#endif 
}

IMPLEMENT_IS_TYPE_B1(Request_chord, Chord);


Request_chord::Request_chord()
{
  multi_level_i_ =0;
}


Musical_pitch 
Music_list::do_relative_octave (Musical_pitch last, bool ret_first)
{

  Musical_pitch retval;
  int count=0;
  for (iter (music_p_list_.top(),i); i.ok (); i++)
    {
      last = i->to_relative_octave (last);
      if (!count ++ )
	retval = last;
    }
  if (!ret_first)
    retval = last;
  return retval;
}

Musical_pitch
Request_chord::to_relative_octave (Musical_pitch last)
{
  for (iter (music_p_list_.top(),i); i.ok (); i++)
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


