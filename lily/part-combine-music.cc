/*   
  part-combine-music.cc --  implement Part_combine_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "part-combine-music.hh"
#include "part-combine-music-iterator.hh"
#include "musical-pitch.hh"

Part_combine_music::Part_combine_music (SCM  what, Music * f, Music * s)
{
  assert (gh_string_p (what));
  set_mus_property ("what", what);
  set_mus_property ("one", f->self_scm ());
  set_mus_property ("two", s->self_scm ());  

  scm_unprotect_object (f->self_scm());
  scm_unprotect_object (s->self_scm());  

  set_mus_property ("type",
		    Part_combine_music_iterator::constructor_cxx_function);
}

void
Part_combine_music::transpose (Musical_pitch p)
{
  first_l ()->transpose (p);
  second_l () ->transpose (p);
}



Moment
Part_combine_music::length_mom () const
{
  return first_l ()->length_mom ();
}

Musical_pitch
Part_combine_music::to_relative_octave (Musical_pitch p)
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
  return unsmob_music (get_mus_property ("one"));
}

Music*
Part_combine_music::second_l () const
{
  return unsmob_music (get_mus_property ("two"));
}
