/*   
  music-sequence.cc --  implement Music_sequence
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"

Music_sequence::Music_sequence (Music_sequence const&s)
  : Music (s)
{
  multi_level_i_ = s.multi_level_i_;
  music_p_list_p_ = new Music_list (*s.music_p_list_p_);
}

IMPLEMENT_IS_TYPE_B1(Music_sequence, Music);

Music_sequence::Music_sequence(Music_list *mlist_p)
{
  multi_level_i_ = 0;
  music_p_list_p_ = mlist_p;
}

void
Music_sequence::transpose (Musical_pitch rq)
{
  for (iter (music_p_list_p_->top(),i); i.ok (); i++)
    i->transpose (rq);    
}

void
Music_sequence::do_print() const
{
#ifndef NPRINT
  for (iter (music_p_list_p_->top(),i); i.ok (); i++)
    i->print();
#endif 
}


void
Music_sequence::add_music (Music *m_p)
{
  music_p_list_p_->add_music (m_p);
}
