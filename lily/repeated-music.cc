/*
  repeated-music.cc -- implement Repeated_music

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "repeated-music.hh"
#include "music-sequence.hh"
#include "pitch.hh"
#include "warn.hh"
#include "program-option.hh"

Music *
Repeated_music::body (Music *me)
{
  return unsmob_music (me->get_property ("element"));
}

SCM
Repeated_music::alternatives (Music *me)
{
  return me->get_property ("elements");
}

MAKE_SCHEME_CALLBACK (Repeated_music, relative_callback, 2);
SCM
Repeated_music::relative_callback (SCM music, SCM pitch)
{
  Pitch p = *unsmob_pitch (pitch);
  Music *me = unsmob_music (music);
  if (lily_1_8_relative)
    {
      Music *body = unsmob_music (me->get_property ("element"));
      if (body)
	p = body->to_relative_octave (p);

      Pitch last = p;
      SCM alternatives = me->get_property ("elements");

      for (SCM s = alternatives; scm_is_pair (s); s = scm_cdr (s))
	{
	  lily_1_8_compatibility_used = true;
	  unsmob_music (scm_car (s))->to_relative_octave (p);
	}

      return last.smobbed_copy ();
    }
  else
    return me->generic_to_relative_octave (p).smobbed_copy ();
}

Moment
Repeated_music::alternatives_get_length (Music *me, bool fold)
{
  SCM alternative_list = alternatives (me);
  int len = scm_ilength (alternative_list);
  if (len <= 0)
    return 0;

  if (fold)
    return Music_sequence::maximum_length (alternative_list);

  Moment m = 0;
  int done = 0;
  int count = robust_scm2int (me->get_property ("repeat-count"), 0);

  SCM p = alternative_list;
  while (scm_is_pair (p) && done < count)
    {
      m = m + unsmob_music (scm_car (p))->get_length ();
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
  if (Music *body = unsmob_music (me->get_property ("element")))
    m = body->get_length ();
  return m;
}

MAKE_SCHEME_CALLBACK (Repeated_music, unfolded_music_length, 1);

SCM
Repeated_music::unfolded_music_length (SCM m)
{
  Music *me = unsmob_music (m);

  Moment l = Moment (repeat_count (me)) * body_get_length (me) + alternatives_get_length (me, false);
  return l.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Repeated_music, folded_music_length, 1);
SCM
Repeated_music::folded_music_length (SCM m)
{
  Music *me = unsmob_music (m);

  Moment l = body_get_length (me) + alternatives_get_length (me, true);
  return l.smobbed_copy ();
}

int
Repeated_music::repeat_count (Music *me)
{
  return scm_to_int (me->get_property ("repeat-count"));
}

MAKE_SCHEME_CALLBACK (Repeated_music, volta_music_length, 1);
SCM
Repeated_music::volta_music_length (SCM m)
{
  Music *me = unsmob_music (m);
  Moment l = body_get_length (me) + alternatives_volta_get_length (me);
  return l.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Repeated_music, minimum_start, 1);
SCM
Repeated_music::minimum_start (SCM m)
{
  Music *me = unsmob_music (m);
  Music *body = unsmob_music (me->get_property ("element"));

  if (body)
    return body->start_mom ().smobbed_copy ();
  else
    return Music_sequence::minimum_start (me->get_property ("elements")).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Repeated_music, first_start, 1);
SCM
Repeated_music::first_start (SCM m)
{
  Music *me = unsmob_music (m);
  Music *body = unsmob_music (me->get_property ("element"));

  Moment rv = (body) ? body->start_mom ()
    : Music_sequence::first_start (me->get_property ("elements"));

  return rv.smobbed_copy ();
}
