/*   
  part-combine-music.cc --  implement Part_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "part-combine-music.hh"
#include "part-combine-music-iterator.hh"
#include "pitch.hh"

Part_combine_music::Part_combine_music (SCM l)
  : Music (l)
{
  set_mus_property ("iterator-ctor",
		    Part_combine_music_iterator::constructor_proc);
}

void
Part_combine_music::transpose (Pitch p)
{
  get_first ()->transpose (p);
  get_second () ->transpose (p);
}

Moment
Part_combine_music::length_mom () const
{
  return get_first ()->length_mom ();
}

Pitch
Part_combine_music::to_relative_octave (Pitch p)
{
  p = get_first ()->to_relative_octave (p);
  return get_second ()->to_relative_octave (p);
}

void
Part_combine_music::compress (Moment m)
{
  get_first ()->compress (m);
  get_second ()->compress (m);
}

Music*
Part_combine_music::get_first () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


Music*
Part_combine_music::get_second () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  l = gh_cdr (l);
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


Part_combine_music::Part_combine_music ()
  : Music (SCM_EOL)
{
}

ADD_MUSIC (Part_combine_music);
