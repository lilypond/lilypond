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

#include "ly-smob-list.hh"
#include "warn.hh"
#include "program-option.hh"
#include "duration.hh"
#include "moment.hh"
#include "music.hh"
#include "input.hh"

void
transpose_music_list (SCM lst, Pitch rq)
{
  for (auto *p : as_ly_smob_list<Prob> (lst))
    if (p)
      p->transpose (rq);
}

Moment
Music_sequence::cumulative_length (SCM l)
{
  Moment cumulative;
  Moment last_len;

  for (auto *mus : as_ly_smob_list<const Music> (l))
    {
      if (!mus)
        programming_error ("Music sequence should have music elements");
      else
        {
          Moment l = mus->get_length ();
          if (last_len.grace_part_ && l.main_part_)
            last_len.grace_part_ = Rational (0);
          cumulative += last_len;
          last_len = l;
        }
    }

  last_len.grace_part_ = Rational (0);
  cumulative += last_len;

  return cumulative;
}

Moment
Music_sequence::maximum_length (SCM l)
{
  Moment dur = 0;
  for (auto *m : as_ly_smob_list<const Music> (l))
    {
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
  return maximum_length (get_property (me, "elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, event_chord_length_callback, 1);
SCM
Music_sequence::event_chord_length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  Duration *d = unsmob<Duration> (get_property (me, "duration"));
  // Preset duration is used in chord repetitions.
  if (d)
    {
      Moment mom (d->get_length ());
      return mom.smobbed_copy ();
    }
  return maximum_length (get_property (me, "elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, cumulative_length_callback, 1);
SCM
Music_sequence::cumulative_length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return cumulative_length (get_property (me, "elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, minimum_start_callback, 1);
SCM
Music_sequence::minimum_start_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return minimum_start (get_property (me, "elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, first_start_callback, 1);
SCM
Music_sequence::first_start_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  return first_start (get_property (me, "elements")).smobbed_copy ();
}

Pitch
music_list_to_relative (SCM l, Pitch p, bool ret_first)
{
  Pitch first = p;
  int count = 0;

  Pitch last = p;
  for (auto *m : as_ly_smob_list<Music> (l))
    {
      if (m)
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
  for (auto *mus : as_ly_smob_list<Music> (l))
    {
      if (!mus)
        programming_error ("Music sequence should have music elements");
      else
        mus->compress (m);
    }
}

Moment
Music_sequence::minimum_start (SCM l)
{
  Moment m;

  for (auto *mus : as_ly_smob_list<const Music> (l))
    {
      if (!mus)
        programming_error ("Music sequence should have music elements");
      else
        m = std::min (m, mus->start_mom ());
    }
  return m;
}

Moment
Music_sequence::first_start (SCM l)
{
  for (auto *mus : as_ly_smob_list<const Music> (l))
    {
      if (!mus)
        {
          programming_error ("Music sequence should have music elements");
          break;
        }

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
  return music_list_to_relative (get_property (me, "elements"),
                                 p, false).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_sequence, event_chord_relative_callback, 2);
SCM
Music_sequence::event_chord_relative_callback (SCM music, SCM pitch)
{
  Music *me = unsmob<Music> (music);
  Pitch p = *unsmob<Pitch> (pitch);
  return music_list_to_relative (get_property (me, "elements"),
                                 p, true).smobbed_copy ();
}
