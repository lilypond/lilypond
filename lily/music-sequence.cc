/*   
  music-sequence.cc --  implement Music_sequence
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"

Music_sequence::Music_sequence (Music_sequence const&s)
  : Music (s)
{
  music_p_list_p_ = new Music_list (*s.music_p_list_p_);
}



Music_sequence::Music_sequence(Music_list *mlist_p)
{
  music_p_list_p_ = mlist_p;
}

void
Music_sequence::transpose (Musical_pitch rq)
{
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    i->car_->transpose (rq);    
}

void
Music_sequence::do_print() const
{
#ifndef NPRINT
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)  
    i->car_->print();
#endif 
}


void
Music_sequence::add_music (Music *m_p)
{
  music_p_list_p_->add_music (m_p);
}

Moment
Music_sequence::cumulative_length () const
{
  Moment last=0;
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    {
      last += i->car_->length_mom ();
    }
  return  last;
}

Musical_pitch
Music_sequence::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, false);
}

Music_iterator*
Music_sequence::to_rhythm (Music_iterator* r)
{
  return do_rhythm (r);
}

Moment
Music_sequence::maximum_length () const
{
  Moment dur = 0;
  for (Cons<Music> *i = music_p_list_p_->head_; i;  i = i->next_)
    dur = dur >? i->car_->length_mom ();

  return dur;
}
int
Music_sequence::length_i () const
{
  return cons_list_size_i (music_p_list_p_->head_);
}
