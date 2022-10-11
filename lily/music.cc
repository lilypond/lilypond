/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "dispatcher.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music-sequence.hh"
#include "score.hh"
#include "warn.hh"
#include "lily-imports.hh"

/*
  Music is anything that has (possibly zero) duration and supports
  both time compression and transposition.

  In Lily, everything that can be thought to have a length and a pitch
  (which has a duration which can be transposed) is considered "music".
*/
bool
Music::internal_is_music_type (SCM k) const
{
  SCM ifs = get_property (this, "types");

  return scm_is_true (scm_c_memq (k, ifs));
}

Preinit_Music::Preinit_Music ()
{
  length_callback_ = SCM_EOL;
  start_callback_ = SCM_EOL;
}

Music::Music (SCM init)
  : Prob (ly_symbol2scm ("Music"), init)
{
  length_callback_ = get_property (this, "length-callback");
  if (!ly_is_procedure (length_callback_))
    length_callback_ = duration_length_callback_proc;

  start_callback_ = get_property (this, "start-callback");
}

void
Music::derived_mark () const
{
  scm_gc_mark (length_callback_);
  scm_gc_mark (start_callback_);
}

SCM
Music::copy_mutable_properties () const
{
  return music_deep_copy (mutable_property_alist_);
}

void
Music::type_check_assignment (SCM s, SCM v) const
{
  ::type_check_assignment (s, v, ly_symbol2scm ("music-type?"));
}

Music::Music (Music const &m)
  : Prob (m)
{
  length_callback_ = m.length_callback_;
  start_callback_ = m.start_callback_;
}

Moment
Music::get_length () const
{
  SCM lst = get_property (this, "length");
  if (auto *mom = unsmob<Moment> (lst))
    return *mom;

  if (ly_is_procedure (length_callback_))
    {
      SCM res = ly_call (length_callback_, self_scm ());
      if (auto *mom = unsmob<Moment> (res))
        return *mom;
    }

  return Moment (0);
}

Moment
Music::start_mom () const
{
  SCM lst = start_callback_;
  if (ly_is_procedure (lst))
    {
      SCM res = ly_call (lst, self_scm ());
      if (auto *mom = unsmob<Moment> (res))
        return *mom;
    }

  return Moment (0);
}

void
print_alist (SCM a, SCM port)
{
  /* SCM_EOL  -> catch malformed lists.  */
  for (SCM s = a; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_display (scm_caar (s), port);
      scm_puts (" = ", port);
      scm_write (scm_cdar (s), port);
      scm_puts ("\n", port);
    }
}

Pitch
Music::generic_to_relative_octave (Pitch last)
{
  SCM elt = get_property (this, "element");
  Pitch *old_pit = unsmob<Pitch> (get_property (this, "pitch"));
  if (old_pit)
    {
      Pitch new_pit = *old_pit;
      new_pit = new_pit.to_relative_octave (last);

      SCM check = get_property (this, "absolute-octave");
      if (scm_is_number (check)
          && new_pit.get_octave () != from_scm<int> (check))
        {
          Pitch expected_pit (from_scm<int> (check), new_pit.get_notename (),
                              new_pit.get_alteration ());
          warning (_f ("octave check failed; expected \"%s\", found: \"%s\"",
                       expected_pit.to_string (), new_pit.to_string ()));
          new_pit = expected_pit;
        }

      set_property (this, "pitch", new_pit.smobbed_copy ());

      last = new_pit;
    }

  if (Music *m = unsmob<Music> (elt))
    last = m->to_relative_octave (last);

  (void) music_list_to_relative (get_property (this, "articulations"), last,
                                 true);
  last = music_list_to_relative (get_property (this, "elements"), last, false);
  return last;
}

Pitch
Music::to_relative_octave (Pitch last)
{
  SCM callback = get_property (this, "to-relative-callback");
  if (ly_is_procedure (callback))
    {
      Pitch *p
        = unsmob<Pitch> (ly_call (callback, self_scm (), last.smobbed_copy ()));
      return *p;
    }

  return generic_to_relative_octave (last);
}

/*
  This mutates alist.  Hence, make sure that it is not shared
*/

void
Prob::transpose (Pitch delta)
{
  if (from_scm<bool> (get_property (this, "untransposable")))
    return;

  for (SCM s = mutable_property_alist_; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM prop = scm_car (entry);
      SCM val = scm_cdr (entry);
      SCM new_val = val;

      if (Pitch *p = unsmob<Pitch> (val))
        {
          Pitch transposed = p->transposed (delta);

          if (scm_is_eq (prop, ly_symbol2scm ("tonic")))
            transposed = Pitch (-1, transposed.get_notename (),
                                transposed.get_alteration ());

          new_val = transposed.smobbed_copy ();
        }
      else if (scm_is_eq (prop, ly_symbol2scm ("element")))
        {
          if (Prob *m = unsmob<Prob> (val))
            m->transpose (delta);
        }
      else if (scm_is_eq (prop, ly_symbol2scm ("elements"))
               || scm_is_eq (prop, ly_symbol2scm ("articulations")))
        transpose_music_list (val, delta);
      else if (scm_is_eq (prop, ly_symbol2scm ("pitch-alist"))
               && scm_is_pair (val))
        new_val = ly_transpose_key_alist (val, delta.smobbed_copy ());

      if (!scm_is_eq (val, new_val))
        scm_set_cdr_x (entry, new_val);
    }
}

void
Music::set_spot (Input ip)
{
  set_property (this, "origin", ip.smobbed_copy ());
}

Input *
Music::origin () const
{
  Input *ip = unsmob<Input> (get_property (this, "origin"));
  return ip ? ip : &dummy_input_global;
}

/*
  ES TODO: This method should probably be reworked or junked.
*/
Stream_event *
Music::to_event () const
{
  SCM class_name
    = ly_camel_case_2_lisp_identifier (get_property (this, "name"));

  // catch programming mistakes.
  if (!internal_is_music_type (class_name))
    programming_error ("Not a music type");

  Stream_event *e = new Stream_event (Lily::ly_make_event_class (class_name),
                                      mutable_property_alist_);
  Moment length = get_length ();
  if (length)
    set_property (e, "length", length.smobbed_copy ());

  // articulations as events.
  SCM art_mus = get_property (e, "articulations");
  if (scm_is_pair (art_mus))
    {
      SCM art_ev = SCM_EOL;
      for (; scm_is_pair (art_mus); art_mus = scm_cdr (art_mus))
        {
          Music *m = unsmob<Music> (scm_car (art_mus));
          art_ev = scm_cons (m->to_event ()->unprotect (), art_ev);
        }
      set_property (e, "articulations", scm_reverse_x (art_ev, SCM_EOL));
    }

  /*
    ES TODO: This is a temporary fix. Stream_events should not be
    aware of music.
  */
  set_property (e, "music-cause", self_scm ());

  return e;
}

void
Music::send_to_context (Context *c)
{
  Stream_event *ev = to_event ();
  c->event_source ()->broadcast (ev);
  ev->unprotect ();
}

Music *
make_music_by_name (SCM sym)
{
  SCM rv = Lily::make_music (sym);

  /* UGH. */
  Music *m = unsmob<Music> (rv);
  m->protect ();
  return m;
}

MAKE_SCHEME_CALLBACK (Music, duration_length_callback,
                      "ly:music::duration-length-callback", 1);
SCM
Music::duration_length_callback (SCM m)
{
  auto *const me = LY_ASSERT_SMOB (Music, m, 1);
  Duration *d = unsmob<Duration> (get_property (me, "duration"));
  Moment mom (d ? d->get_length () : 0);
  return mom.smobbed_copy ();
}

SCM
music_deep_copy (SCM m)
{
  if (Music *mus = unsmob<Music> (m))
    return mus->clone ()->unprotect ();
  if (scm_is_pair (m))
    {
      SCM copy = SCM_EOL;
      do
        {
          copy = scm_cons (music_deep_copy (scm_car (m)), copy);
          m = scm_cdr (m);
        }
      while (scm_is_pair (m));
      // Oh, come on, GUILE.  Why do you require the second argument
      // of scm_reverse_x to be a proper list?  That makes no sense.
      // return scm_reverse_x (copy, music_deep_copy (m));
      SCM last_cons = copy;
      copy = scm_reverse_x (copy, SCM_EOL);
      scm_set_cdr_x (last_cons, music_deep_copy (m));
      return copy;
    }
  return m;
}

void
set_origin (SCM m, SCM origin)
{
  while (scm_is_pair (m))
    {
      set_origin (scm_car (m), origin);
      m = scm_cdr (m);
    }
  if (Music *mus = unsmob<Music> (m))
    set_property (mus, "origin", origin);
}
