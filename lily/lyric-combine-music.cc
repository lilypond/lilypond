/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"
#include "pitch.hh"

Lyric_combine_music::Lyric_combine_music ()
  : Music ()
{
}

Moment
Lyric_combine_music::get_length () const
{
  return get_music ()->get_length ();
}

Music*
Lyric_combine_music::get_music () const
{
  SCM l = get_property ("elements");
  if (!ly_c_pair_p (l))
    return 0;
  return unsmob_music (ly_car (l));
}


Music*
Lyric_combine_music::get_lyrics () const
{
  SCM l = get_property ("elements");
  if (!ly_c_pair_p (l))
    return 0;
  l = ly_cdr (l);
  if (!ly_c_pair_p (l))
    return 0;
  return unsmob_music (ly_car (l));
}


ADD_MUSIC (Lyric_combine_music);
