/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "music-sequence.hh"

#include "warn.hh"
#include "program-option.hh"
#include "music.hh"
#include "input.hh"

void
transpose_music_list (SCM lst, Pitch rq)
{
  for (SCM s = lst; scm_is_pair (s); s = scm_cdr (s))
    unsmob_music (scm_car (s))->transpose (rq);
}

Moment
Music_sequence::cumulative_length (SCM l)
{
  Moment cumulative;
  Moment last_len;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      Moment l = unsmob_music (scm_car (s))->get_length ();
      if (last_len.grace_part_ && l.main_part_)
	last_len.grace_part_ = Rational (0);
      cumulative += last_len;
      last_len = l;
    }

  last_len.grace_part_ = Rational (0);
  cumulative += last_len;

  return cumulative;
}

Moment
Music_sequence::maximum_length (SCM l)
{
  Moment dur = 0;
  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      Music *m = unsmob_music (scm_car (s));
      if (!m)
	programming_error ("Music sequence should have music elements");
      else
	{
	  Moment l = m->get_length ();
	  dur = max (dur, l);
	}
    }

  return dur;
}

MAKE_SCHEME_CALLBACK (Music_sequence, maximum_length_callback, 1);
SCM
Music_sequence::maximum_length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  return maximum_length (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, cumulative_length_callback, 1);
SCM
Music_sequence::cumulative_length_callback (SCM m)
{
  Music *me = unsmob_music (m);
  return cumulative_length (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, minimum_start_callback, 1);
SCM
Music_sequence::minimum_start_callback (SCM m)
{
  Music *me = unsmob_music (m);
  return minimum_start (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, first_start_callback, 1);
SCM
Music_sequence::first_start_callback (SCM m)
{
  Music *me = unsmob_music (m);
  return first_start (me->get_property ("elements")).smobbed_copy ();
}

Pitch
music_list_to_relative (SCM l, Pitch p, bool ret_first)
{
  Pitch first = p;
  int count = 0;

  Pitch last = p;
  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Music *m = unsmob_music (scm_car (s)))
	{
	  last = m->to_relative_octave (last);
	  if (!count++)
	    first = last;
	}
    }

  return (ret_first) ? first : last;
}

void
compress_music_list (SCM l, Moment m)
{
  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    unsmob_music (scm_car (s))->compress (m);
}

Moment
Music_sequence::minimum_start (SCM l)
{
  Moment m;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    m = min (m, unsmob_music (scm_car (s))->start_mom ());
  return m;
}

Moment
Music_sequence::first_start (SCM l)
{
  Moment m;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      Music *mus = unsmob_music (scm_car (s));
      Moment l = mus->get_length ();
      Moment s = mus->start_mom ();
      if (l.to_bool () || s.to_bool ())
	return s;
    }
  return m;
}

MAKE_SCHEME_CALLBACK (Music_sequence, simultaneous_relative_callback, 2);
SCM
Music_sequence::simultaneous_relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob_music (music);
  Pitch p = *unsmob_pitch (pitch);

  SCM elts = me->get_property ("elements");
  SCM copied = SCM_EOL;
  if (lily_1_8_relative)
    copied = ly_music_deep_copy (elts);

  Pitch retval = music_list_to_relative (elts, p, false);

  if (lily_1_8_relative)
    {

      Pitch retval_1_8 = music_list_to_relative (copied, p, true);
      if (retval_1_8 != retval)
	lily_1_8_compatibility_used = true;

      retval = retval_1_8;
    }

  return retval.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, event_chord_relative_callback, 2);
SCM
Music_sequence::event_chord_relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob_music (music);
  Pitch p = *unsmob_pitch (pitch);
  return music_list_to_relative (me->get_property ("elements"),
				 p, true).smobbed_copy ();
}

