/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"
#include "musical-pitch.hh"

Lyric_combine_music::Lyric_combine_music (Music * m, Music * l)
{
  set_mus_property ("music", m->self_scm ());
  set_mus_property ("lyrics", l->self_scm ());  
}


void
Lyric_combine_music::transpose (Musical_pitch p)
{
  music_l ()->transpose (p);
  lyrics_l () ->transpose (p);
}


Moment
Lyric_combine_music::length_mom () const
{
  return music_l ()->length_mom ();
}

Musical_pitch
Lyric_combine_music::to_relative_octave (  Musical_pitch p )
{
  p = music_l ()->to_relative_octave (p);
  return lyrics_l () ->to_relative_octave (p);
}

void
Lyric_combine_music::compress (Moment m)
{
  music_l ()->compress (m);
}

Music*
Lyric_combine_music::music_l () const
{
  return unsmob_music (get_mus_property ("music"));
}

Music*
Lyric_combine_music::lyrics_l () const
{
  return unsmob_music (get_mus_property ("lyrics"));
}
