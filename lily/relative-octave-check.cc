#include "relative-octave-check.hh"

#include "input.hh"
#include "pitch.hh"

/*
  yeah, so this should be in a separate file.
  I'm lazy.
 */
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

ADD_MUSIC (Relative_octave_check);
