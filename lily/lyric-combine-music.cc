/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"
#include "musical-pitch.hh"

Lyric_combine_music::Lyric_combine_music (Music * m, Music * l)
{
  music_p_ = m;
  lyrics_p_ = l;
}

Lyric_combine_music::~Lyric_combine_music ()
{
  delete music_p_;
  delete lyrics_p_;
}

Lyric_combine_music::Lyric_combine_music (Lyric_combine_music const&s)
  : Music (s)
{
  music_p_ = s.music_p_ ? s.music_p_->clone ():0;
  lyrics_p_ = s.lyrics_p_ ? s.lyrics_p_->clone ():0;
}

void
Lyric_combine_music::transpose (Musical_pitch p)
{
  music_p_->transpose (p);
  lyrics_p_->transpose (p);
}

void
Lyric_combine_music::do_print () const  
{
  music_p_->print();
  lyrics_p_->print ();
}

Moment
Lyric_combine_music::length_mom () const
{
  return music_p_->length_mom ();
}

Musical_pitch
Lyric_combine_music::to_relative_octave (  Musical_pitch p )
{
  p = music_p_->to_relative_octave (p);
  return lyrics_p_->to_relative_octave (p);
}

void
Lyric_combine_music::compress (Moment m)
{
  music_p_->compress (m);
}

Music*
Lyric_combine_music::music_l () const
{
  return music_p_;
}

Music*
Lyric_combine_music::lyrics_l () const
{
  return lyrics_p_;
}
