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
		    Part_combine_music_iterator::constructor_cxx_function);
}

void
Part_combine_music::transpose (Pitch p)
{
  first_l ()->transpose (p);
  second_l () ->transpose (p);
}

Moment
Part_combine_music::length_mom () const
{
  return first_l ()->length_mom ();
}

Pitch
Part_combine_music::to_relative_octave (Pitch p)
{
  p = first_l ()->to_relative_octave (p);
  return second_l ()->to_relative_octave (p);
}

void
Part_combine_music::compress (Moment m)
{
  first_l ()->compress (m);
  second_l ()->compress (m);
}

Music*
Part_combine_music::first_l () const
{
  SCM l = get_mus_property ("elements");
  if (!gh_pair_p (l))
    return 0;
  return unsmob_music (gh_car (l));
}


Music*
Part_combine_music::second_l () const
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
