/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"
#include "lyric-combine-music-iterator.hh"
#include "pitch.hh"

Lyric_combine_music::Lyric_combine_music (SCM l)
  : Music (l)
{
  set_mus_property ("iterator-ctor",
		    Lyric_combine_music_iterator::constructor_cxx_function);
}


void
Lyric_combine_music::transpose (Pitch p)
{
  music_l ()->transpose (p);
  lyrics_l () ->transpose (p);
}


Moment
Lyric_combine_music::length_mom () const
{
  return music_l ()->length_mom ();
}

Pitch
Lyric_combine_music::to_relative_octave (Pitch p)
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
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


Music*
Lyric_combine_music::lyrics_l () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  l = gh_cdr (l);
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}

Lyric_combine_music::Lyric_combine_music ()
  : Music (SCM_EOL)
{
}

ADD_MUSIC (Lyric_combine_music);
