/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music.hh"

#include "duration.hh"
#include "input.hh"
#include "lily-imports.hh"
#include "program-option.hh"
#include "warn.hh"

LY_DEFINE (ly_music_length, "ly:music-length",
           1, 0, 0, (SCM mus),
           "Get the length of music expression @var{mus} and return"
           " it as a @code{Moment} object.")
{
  auto *const sc = LY_ASSERT_SMOB (Music, mus, 1);
  return sc->get_length ().smobbed_copy ();
}

LY_DEFINE (ly_music_property, "ly:music-property",
           2, 1, 0, (SCM mus, SCM sym, SCM val),
           "Return the value for property @var{sym} of music expression"
           " @var{mus}.  If no value is found, return @var{val} or"
           " @code{'()} if @var{val} is not specified.")
{
  LY_ASSERT_SMOB (Music, mus, 1);
  return ly_prob_property (mus, sym, val);
}

LY_DEFINE (ly_music_set_property_x, "ly:music-set-property!",
           3, 0, 0, (SCM mus, SCM sym, SCM val),
           "Set property @var{sym} in music expression @var{mus} to"
           " @var{val}.")
{
  LY_ASSERT_SMOB (Music, mus, 1);

  return ly_prob_set_property_x (mus, sym, val);
}

LY_DEFINE (ly_music_start, "ly:music-start",
           1, 0, 0, (SCM mus),
           "Get the start of music expression @var{mus} and return"
           " it as a @code{Moment} object.")
{
  auto *const sc = LY_ASSERT_SMOB (Music, mus, 1);
  return sc->start_mom ().smobbed_copy ();
}

/* todo:  property args */
LY_DEFINE (ly_make_music, "ly:make-music",
           1, 0, 0, (SCM props),
           "Make a C++ @code{Music} object and initialize it with"
           " @var{props}.\n"
           "\n"
           "This function is for internal use and is only called by"
           " @code{make-music}, which is the preferred interface"
           " for creating music objects.")
{
  Music *ms = new Music (props);
  return ms->unprotect ();
}

LY_DEFINE (ly_music_p, "ly:music?",
           1, 0, 0, (SCM obj),
           "Is @var{obj} a @code{Music} object?")
{
  return ly_bool2scm (unsmob<Music> (obj));
}

LY_DEFINE (ly_event_p, "ly:event?",
           1, 0, 0, (SCM obj),
           "Is @var{obj} a proper (non-rhythmic) @code{Event} object?")
{
  if (Music *m = unsmob<Music> (obj))
    {
      return scm_from_bool (m->is_mus_type ("post-event"));
    }
  return SCM_BOOL_F;
}

/* todo: property args */
LY_DEFINE (ly_music_mutable_properties, "ly:music-mutable-properties",
           1, 0, 0, (SCM mus),
           "Return an alist containing the mutable properties of @var{mus}."
           "  The immutable properties are not available, since they are"
           " constant and initialized by the @code{make-music} function.")
{
  auto *const m = LY_ASSERT_SMOB (Music, mus, 1);
  return m->get_property_alist (true);
}

LY_DEFINE (ly_music_list_p, "ly:music-list?",
           1, 0, 0, (SCM lst),
           "Is @var{lst} a list of music objects?")
{
  if (!ly_is_list (lst))
    return SCM_BOOL_F;

  while (scm_is_pair (lst))
    {
      if (!unsmob<Music> (scm_car (lst)))
        return SCM_BOOL_F;
      lst = scm_cdr (lst);
    }

  return SCM_BOOL_T;
}

LY_DEFINE (ly_music_deep_copy, "ly:music-deep-copy",
           1, 1, 0, (SCM m, SCM origin),
           "Copy @var{m} and all sub expressions of@tie{}@var{m}."
           " @var{m} may be an arbitrary type; cons cells and music"
           " are copied recursively.  If @var{origin} is given,"
           " it is used as the origin for one level of music by calling"
           " @code{ly:set-origin!} on the copy.")
{
  m = music_deep_copy (m);

  if (SCM_UNBNDP (origin))
    return m;

  if (Music *mus = unsmob<Music> (origin))
    origin = get_property (mus, "origin");

  if (scm_is_false (origin) || scm_is_null (origin))
    return m;

  LY_ASSERT_SMOB (Input, origin, 2);

  set_origin (m, origin);
  return m;
}

LY_DEFINE (ly_set_origin_x, "ly:set-origin!",
           1, 1, 0, (SCM m, SCM origin),
           "This sets the origin given in @var{origin} to @var{m}. "
           " @var{m} will typically be a music expression or a list"
           " of music.  List structures are searched recursively,"
           " but recursion stops at the changed music expressions"
           " themselves. "
           " @var{origin} is generally of type @code{ly:input-location?},"
           " defaulting to @code{(*location*)}.  Other valid values for"
           " @code{origin} are a music expression which is then used as"
           " the source of location information, or @code{#f}"
           " or @code{'()} in which case no action is performed. "
           " The return value is @var{m} itself.")
{
  if (SCM_UNBNDP (origin))
    origin = scm_fluid_ref (Lily::f_location);
  else if (Music *mus = unsmob<Music> (origin))
    origin = get_property (mus, "origin");

  if (scm_is_false (origin) || scm_is_null (origin))
    return m;

  LY_ASSERT_SMOB (Input, origin, 2);

  set_origin (m, origin);
  return m;
}

LY_DEFINE (ly_music_transpose, "ly:music-transpose",
           2, 0, 0, (SCM m, SCM p),
           "Transpose @var{m} such that central@tie{}C is mapped"
           " to@tie{}@var{p}.  Return@tie{}@var{m}.")
{
  auto *const sc = LY_ASSERT_SMOB (Music, m, 1);
  auto *const sp = LY_ASSERT_SMOB (Pitch, p, 2);

  sc->transpose (*sp);
  // SCM_UNDEFINED ?
  return sc->self_scm ();
}

LY_DEFINE (ly_music_compress, "ly:music-compress",
           2, 0, 0, (SCM m, SCM factor),
           "Compress music object@tie{}@var{m} by scale @var{factor}.")
{
  auto *const sc = LY_ASSERT_SMOB (Music, m, 1);
  SCM_ASSERT_TYPE (scm_is_true (Lily::scale_p (factor)),
                   factor, SCM_ARG2, __FUNCTION__,
                   "non-negative rational, fraction, or moment");

  sc->compress (from_scm<Rational> (Lily::scale_to_factor (factor)));
  return m;
}

LY_DEFINE (ly_make_music_relative_x, "ly:make-music-relative!",
           2, 0, 0, (SCM music, SCM pitch),
           "Make @var{music} relative to @var{pitch},"
           " return final pitch.")
{
  auto *const m = LY_ASSERT_SMOB (Music, music, 1);
  auto start = *LY_ASSERT_SMOB (Pitch, pitch, 2);

  Pitch last = m->to_relative_octave (start);

  return last.smobbed_copy ();
}

LY_DEFINE (ly_music_duration_length, "ly:music-duration-length", 1, 0, 0,
           (SCM mus),
           "Extract the duration field from @var{mus} and return the"
           " length.")
{
  auto *const m = LY_ASSERT_SMOB (Music, mus, 1);

  Duration *d = unsmob<Duration> (get_property (m, "duration"));
  Moment len;

  if (d)
    len = Moment (d->get_length ());
  else
    programming_error ("music has no duration");
  return len.smobbed_copy ();
}

LY_DEFINE (ly_music_duration_compress, "ly:music-duration-compress", 2, 0, 0,
           (SCM mus, SCM fact),
           "Compress @var{mus} by factor @var{fact}, which is a"
           " @code{Moment}.")
{
  auto *const m = LY_ASSERT_SMOB (Music, mus, 1);
  auto *const f = LY_ASSERT_SMOB (Moment, fact, 2);

  Duration *d = unsmob<Duration> (get_property (m, "duration"));
  if (d)
    set_property (m, "duration", d->compressed (f->main_part_).smobbed_copy ());
  return SCM_UNSPECIFIED;
}

/*
  This is hairy, since the scale in a key-change event may contain
  octaveless notes.


  TODO: this should use ly:pitch.
*/
LY_DEFINE (ly_transpose_key_alist, "ly:transpose-key-alist",
           2, 0, 0, (SCM l, SCM pit),
           "Make a new key alist of@tie{}@var{l} transposed by"
           " pitch @var{pit}.")
{
  SCM newlist = SCM_EOL;
  Pitch p (*LY_ASSERT_SMOB (Pitch, pit, 2));

  for (SCM s = l; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM alter = scm_cdar (s);
      if (scm_is_pair (key))
        {
          Pitch orig (scm_to_int (scm_car (key)),
                      scm_to_int (scm_cdr (key)),
                      from_scm<Rational> (alter));

          orig = orig.transposed (p);

          SCM key = scm_cons (to_scm (orig.get_octave ()),
                              to_scm (orig.get_notename ()));

          newlist = scm_cons (scm_cons (key, to_scm (orig.get_alteration ())),
                              newlist);
        }
      else if (scm_is_number (key))
        {
          Pitch orig (0, scm_to_int (key), from_scm<Rational> (alter));
          orig = orig.transposed (p);

          key = to_scm (orig.get_notename ());
          alter = to_scm (orig.get_alteration ());
          newlist = scm_cons (scm_cons (key, alter), newlist);
        }
    }
  return scm_reverse_x (newlist, SCM_EOL);
}
