/*
  music-list.cc -- implement Music_sequence, Simultaneous_music, Sequential_music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "music-wrapper.hh"
#include "musical-pitch.hh"
#include "request.hh"
#include "musical-request.hh"
#include "music-iterator.hh"
#include "main.hh"
#include "killing-cons.tcc"

Moment
Simultaneous_music::length_mom () const
{
  return maximum_length ();
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
  return cumulative_length ();
}

Musical_pitch
Simultaneous_music::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, true);
}

Music_iterator*
Simultaneous_music::to_rhythm (Music_iterator* r)
{
  return do_rhythm (r);
}

Musical_pitch
Music_sequence::do_relative_octave (Musical_pitch p, bool b)
{
  return music_p_list_p_->do_relative_octave (p, b);  
}

Music_iterator*
Music_sequence::do_rhythm (Music_iterator* r)
{
  return music_p_list_p_->do_rhythm (r);
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

Music_iterator*
Music_list::do_rhythm (Music_iterator* r)
{
  for (Cons<Music> *i = head_; i ; i = i->next_)
    {
      r = i->car_->to_rhythm (r);
    }
  return r;
}

Music_list::Music_list (Music_list const &s)
  : Cons_list<Music> (s)
{
  Cons_list<Music>::init ();
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

Music_iterator*
Request_chord::to_rhythm (Music_iterator* it)
{
  for (Cons<Music>* i = music_p_list_p_->head_; i ; i = i->next_)
    {
      if (Rhythmic_req* r= dynamic_cast <Rhythmic_req*> (i->car_))
	{
	  for (Music*m = it->next_music_l (); m; m = it->next_music_l ())
	    {
#if 0
	      // is it sane to assume we don't want rests on lyrics/in rhythm?
	      if (dynamic_cast <Rest_req*> (r)
		  || dynamic_cast <Multi_measure_rest_req*> (r)
		  || dynamic_cast <Skip_req*> (r))
		  {
		    continue;
		  }
#endif
	      if (Rhythmic_req* d= dynamic_cast <Rhythmic_req*> (m))
		{
		  r->duration_ = d->duration_;
		  return it;
		}
	    }
	}
    }
  return it;
}

Music_list::Music_list ()
{
}

Music_sequence::~Music_sequence ()
{
  delete music_p_list_p_;
}
