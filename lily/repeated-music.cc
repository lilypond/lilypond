/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "repeated-music.hh"
#include "music-sequence.hh"
#include "pitch.hh"
#include "warn.hh"
#include "program-option.hh"

Music *
Repeated_music::body (Music *me)
{
  return unsmob<Music> (get_property (me, "element"));
}

SCM
Repeated_music::alternatives (Music *me)
{
  return get_property (me, "elements");
}

Moment
Repeated_music::alternatives_get_length (Music *me, bool fold)
{
  SCM alternative_list = alternatives (me);
  long len = scm_ilength (alternative_list);
  if (len <= 0)
    return 0;

  if (fold)
    return Music_sequence::maximum_length (alternative_list);

  Moment m = 0;
  int done = 0;
  int count = robust_scm2int (get_property (me, "repeat-count"), 0);

  SCM p = alternative_list;
  while (scm_is_pair (p) && done < count)
    {
      m = m + unsmob<Music> (scm_car (p))->get_length ();
      done++;
      if (count - done < len)
        p = scm_cdr (p);
    }
  return m;
}

/*
  Sum all duration of all available alternatives. This is for the case
  of volta repeats, where the alternatives are iterated just as they
  were entered.  */
Moment
Repeated_music::alternatives_volta_get_length (Music *me)
{
  return Music_sequence::cumulative_length (alternatives (me));
}

/*
  Length of the body in THIS. Disregards REPEAT-COUNT.
*/
Moment
Repeated_music::body_get_length (Music *me)
{
  Moment m = 0;
  if (Music *body = unsmob<Music> (get_property (me, "element")))
    m = body->get_length ();
  return m;
}

MAKE_SCHEME_CALLBACK (Repeated_music, unfolded_music_length, 1);

SCM
Repeated_music::unfolded_music_length (SCM m)
{
  Music *me = unsmob<Music> (m);

  // TODO: This smells fishy.  Is the result supposed to include any grace
  // time?  If not, we should discard grace time from the alternatives as from
  // the body.  If so, it seems improper to sum the grace time; shouldn't we
  // report 1x the body grace time (for just the first pass) as the grace time
  // of the unfolded music?
  Moment l = (Moment (repeat_count (me)) * body_get_length (me).main_part_)
    + alternatives_get_length (me, false);
  return l.smobbed_copy ();
}

int
Repeated_music::repeat_count (Music *me)
{
  return scm_to_int (get_property (me, "repeat-count"));
}

MAKE_SCHEME_CALLBACK (Repeated_music, volta_music_length, 1);
SCM
Repeated_music::volta_music_length (SCM m)
{
  Music *me = unsmob<Music> (m);
  Moment l = body_get_length (me) + alternatives_volta_get_length (me);
  return l.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Repeated_music, minimum_start, 1);
SCM
Repeated_music::minimum_start (SCM m)
{
  Music *me = unsmob<Music> (m);
  Music *body = unsmob<Music> (get_property (me, "element"));

  if (body)
    return body->start_mom ().smobbed_copy ();
  else
    return Music_sequence::minimum_start (get_property (me, "elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Repeated_music, first_start, 1);
SCM
Repeated_music::first_start (SCM m)
{
  Music *me = unsmob<Music> (m);
  Music *body = unsmob<Music> (get_property (me, "element"));

  Moment rv = (body) ? body->start_mom ()
              : Music_sequence::first_start (get_property (me, "elements"));

  return rv.smobbed_copy ();
}
