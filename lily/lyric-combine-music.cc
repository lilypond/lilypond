/*   
  lyric-combine-music.cc --  implement Lyric_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "lyric-combine-music.hh"

#include "pitch.hh"

Lyric_combine_music::Lyric_combine_music (SCM x)
  : Music (x)
{
}

MAKE_SCHEME_CALLBACK(Lyric_combine_music,length_callback,1);
SCM
Lyric_combine_music::length_callback (SCM m)
{
  Music * me = unsmob_music (m);
  Music *melody = unsmob_music (scm_car (me->get_property ("elements")));
  return melody->get_length ().smobbed_copy ();
}

Music*
Lyric_combine_music::get_music () const
{
  SCM l = get_property ("elements");
  if (!scm_is_pair (l))
    return 0;
  return unsmob_music (scm_car (l));
}


Music*
Lyric_combine_music::get_lyrics () const
{
  SCM l = get_property ("elements");
  if (!scm_is_pair (l))
    return 0;
  l = scm_cdr (l);
  if (!scm_is_pair (l))
    return 0;
  return unsmob_music (scm_car (l));
}


ADD_MUSIC (Lyric_combine_music);
