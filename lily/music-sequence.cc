/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "duration.hh"
#include "input.hh"
#include "moment.hh"
#include "music.hh"
#include "program-option.hh"
#include "warn.hh"

void
transpose_music_list (SCM lst, Pitch rq)
{
  for (SCM s = lst; scm_is_pair (s); s = scm_cdr (s))
    if (Prob *p = unsmob<Prob> (scm_car (s)))
      p->transpose (rq);
}

Moment
Music_sequence::cumulative_length (SCM l)
{
  Moment cumulative;
  Moment last_len;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      Moment l = unsmob<Music> (scm_car (s))->get_length ();
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
      Music *m = unsmob<Music> (scm_car (s));
      if (!m)
        programming_error ("Music sequence should have music elements");
      else
        {
          Moment l = m->get_length ();
          dur = std::max (dur, l);
        }
    }

  return dur;
}

MAKE_SCHEME_CALLBACK (Music_sequence, maximum_length_callback, 1);
SCM
Music_sequence::maximum_length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return maximum_length (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, event_chord_length_callback, 1);
SCM
Music_sequence::event_chord_length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  Duration *d = unsmob<Duration> (me->get_property ("duration"));
  // Preset duration is used in chord repetitions.
  if (d)
    {
      Moment mom = d->get_length ();
      return mom.smobbed_copy ();
    }
  return maximum_length (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, cumulative_length_callback, 1);
SCM
Music_sequence::cumulative_length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return cumulative_length (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, minimum_start_callback, 1);
SCM
Music_sequence::minimum_start_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return minimum_start (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, first_start_callback, 1);
SCM
Music_sequence::first_start_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
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
      if (Music *m = unsmob<Music> (scm_car (s)))
        {
          last = m->to_relative_octave (last);
          if (!count++)
            first = last;
        }
    }

  return (ret_first) ? first : last;
}

void
compress_music_list (SCM l, Rational m)
{
  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    unsmob<Music> (scm_car (s))->compress (m);
}

Moment
Music_sequence::minimum_start (SCM l)
{
  Moment m;

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    m = std::min (m, unsmob<Music> (scm_car (s))->start_mom ());
  return m;
}

Moment
Music_sequence::first_start (SCM l)
{

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      Music *mus = unsmob<Music> (scm_car (s));
      Moment start = mus->start_mom ();
      if (mus->get_length ().to_bool () || start.to_bool ())
        return start;
    }
  return Moment ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, simultaneous_relative_callback, 2);
SCM
Music_sequence::simultaneous_relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob<Music> (music);
  Pitch p = *unsmob<Pitch> (pitch);
  return music_list_to_relative (me->get_property ("elements"), p, false)
      .smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, event_chord_relative_callback, 2);
SCM
Music_sequence::event_chord_relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob<Music> (music);
  Pitch p = *unsmob<Pitch> (pitch);
  return music_list_to_relative (me->get_property ("elements"), p, true)
      .smobbed_copy ();
}
