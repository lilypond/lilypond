/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"
#include "lyric-combine-music-iterator.hh"
#include "pitch.hh"

Lyric_combine_music::Lyric_combine_music ()
  : Music ()
{
}


void
Lyric_combine_music::transpose (Pitch p)
{
  get_music ()->transpose (p);
  get_lyrics () ->transpose (p);
}


Moment
Lyric_combine_music::get_length () const
{
  return get_music ()->get_length ();
}

Pitch
Lyric_combine_music::to_relative_octave (Pitch p)
{
  p = get_music ()->to_relative_octave (p);
  return get_lyrics () ->to_relative_octave (p);
}

void
Lyric_combine_music::compress (Moment m)
{
  get_music ()->compress (m);
}



Music*
Lyric_combine_music::get_music () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


Music*
Lyric_combine_music::get_lyrics () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  l = gh_cdr (l);
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


ADD_MUSIC (Lyric_combine_music);
