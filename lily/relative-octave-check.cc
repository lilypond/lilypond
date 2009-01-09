/*
  relative-octave-check.cc -- implement Relative_octave_check

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "input.hh"
#include "international.hh"
#include "music.hh"

class Relative_octave_check
{
public:
  DECLARE_SCHEME_CALLBACK (relative_callback, (SCM, SCM));
};

MAKE_SCHEME_CALLBACK (Relative_octave_check, relative_callback, 2)
  SCM
Relative_octave_check::relative_callback (SCM music, SCM last_pitch)
{
  Pitch p = *unsmob_pitch (last_pitch);
  Music *m = unsmob_music (music);
  Pitch *check_p = unsmob_pitch (m->get_property ("pitch"));

  int delta_oct = 0;
  if (check_p)
    {
      Pitch no_octave (-1,
		       check_p->get_notename (),
		       check_p->get_alteration ());

      Pitch result = no_octave.to_relative_octave (p);

      if (result != *check_p)
	{
	  string s = _ ("Failed octave check, got: ");
	  s += result.to_string ();

	  m->origin ()->warning (s);

	  delta_oct = check_p->get_octave () - result.get_octave ();
	}
    }

  return Pitch (p.get_octave () + delta_oct,
		p.get_notename (),
		p.get_alteration ()).smobbed_copy ();
}
