/*
  event.cc -- implement Event

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "event.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK(Event,length_callback,1);
SCM
Event::length_callback (SCM m)
{
  Music* me = unsmob_music (m);
  Duration *d = unsmob_duration (me->get_property ("duration"));

  Moment mom;
  if (d)
    {
      mom = d->get_length ();
    }
  return mom.smobbed_copy();
}

void
Event::compress (Moment m)
{
  Duration *d =  unsmob_duration (get_property ("duration"));
  if (d)
    set_property ("duration", d ->compressed (m.main_part_).smobbed_copy ());
}


Pitch
Event::to_relative_octave (Pitch last)
{
  Pitch *old_pit = unsmob_pitch (get_property ("pitch"));
  if (old_pit)
    {
      Pitch new_pit = *old_pit;
      new_pit = new_pit.to_relative_octave (last);

      SCM check = get_property ("absolute-octave");
      if (scm_is_number (check) &&
	  new_pit.get_octave () != scm_to_int (check))
	{
	  Pitch expected_pit (scm_to_int (check),
			      new_pit.get_notename (),
			      new_pit.get_alteration ());
	  origin ()->warning (_f ("octave check failed; expected %s, found: %s",
				  expected_pit.to_string (),
				  new_pit.to_string ()));
	  new_pit = expected_pit;
	}
      
      set_property ("pitch", new_pit.smobbed_copy ());
  
      return new_pit;
    }
  return last;
}
  
Event::Event (SCM i)
  : Music (i)
{
  if (!ly_c_procedure_p (length_callback_))
    {
      length_callback_ = length_callback_proc;
    }
}

ADD_MUSIC (Event);

Key_change_ev::Key_change_ev (SCM x)
  : Event (x)
{
}
void
Key_change_ev::transpose (Pitch p)
{
  SCM pa = get_property ("pitch-alist");
  set_property ("pitch-alist", ly_transpose_key_alist (pa, p.smobbed_copy ()));

  Event::transpose (p);
}

bool
alist_equal_p (SCM a, SCM b)
{
  for (SCM s = a;
       scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM val = scm_cdar (s);
      SCM l = scm_assoc (key, b);

      if (l == SCM_BOOL_F
	  || !ly_c_equal_p ( scm_cdr (l), val))

	return false;
    }
  return true;
}
ADD_MUSIC (Key_change_ev);
