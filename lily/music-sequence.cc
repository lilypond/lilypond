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
