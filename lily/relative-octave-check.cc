/*
  relative-octave-check.cc --  implement Relative_octave_check

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "relative-octave-check.hh"

#include "input.hh"
#include "pitch.hh"

Pitch
Relative_octave_check::to_relative_octave (Pitch p)
{
  Pitch * check_p = unsmob_pitch (get_property ("pitch"));

  int delta_oct = 0;
  if (check_p)
    {
      Pitch no_octave (-1,
		       check_p->get_notename (),
		       check_p->get_alteration ());

      Pitch result = no_octave.to_relative_octave (p);

      if (result != *check_p)
	{
	  String s = _("Failed octave check, got: ");
	  s += result.to_string ();
	  
	  origin ()->warning (s);
	  
	  delta_oct = check_p->get_octave () - result.get_octave ();
	}
    }
  
  return  Pitch (p.get_octave () + delta_oct,
		 p.get_notename (), p.get_alteration ());
}


Relative_octave_check::Relative_octave_check (SCM x)
  : Music (x)
{
}

ADD_MUSIC (Relative_octave_check);
