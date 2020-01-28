/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Erik Sandberg  <mandolaerik@gmail.com>

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

#include "stream-event.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "pitch.hh"
#include <string>

/* TODO: Rename Stream_event -> Event */

Stream_event::Stream_event () : Prob (ly_symbol2scm ("Stream_event"), SCM_EOL)
{
}

Stream_event::Stream_event (SCM event_class, SCM immutable_props)
    : Prob (ly_symbol2scm ("Stream_event"),
            scm_acons (ly_symbol2scm ("class"), event_class, immutable_props))
{
}

Stream_event::Stream_event (SCM class_name, Input *origin)
    : Prob (ly_symbol2scm ("Stream_event"),
            scm_list_1 (scm_cons (ly_symbol2scm ("class"), class_name)))
{
  if (origin)
    set_spot (origin);
}

SCM
Stream_event::copy_mutable_properties () const
{
  return ly_event_deep_copy (mutable_property_alist_);
}

Input *
Stream_event::origin () const
{
  Input *i = unsmob<Input> (get_property ("origin"));
  return i ? i : &dummy_input_global;
}

void
Stream_event::set_spot (Input *i)
{
  set_property ("origin", i->smobbed_copy ());
}

bool
Stream_event::internal_in_event_class (SCM class_name)
{
  SCM cl = get_property ("class");
  return scm_is_true (scm_c_memq (class_name, cl));
}

MAKE_SCHEME_CALLBACK (Stream_event, undump, 1);
MAKE_SCHEME_CALLBACK (Stream_event, dump, 1);

void
Stream_event::make_transposable ()
{
  /* This is in preparation for transposing stuff
     that may be defined in the immutable part */

  for (SCM s = immutable_property_alist_; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      SCM prop = scm_car (entry);
      SCM val = scm_cdr (entry);

      if ((unsmob<Pitch> (val)
           || (scm_is_eq (prop, ly_symbol2scm ("element"))
               && unsmob<Music> (val))
           || (scm_is_eq (prop, ly_symbol2scm ("elements"))
               && scm_is_pair (val))
           || (scm_is_eq (prop, ly_symbol2scm ("pitch-alist"))
               && scm_is_pair (val)))
          && scm_is_false (scm_assq (prop, mutable_property_alist_)))
        mutable_property_alist_
            = scm_acons (prop, music_deep_copy (val), mutable_property_alist_);
    }
}

SCM
Stream_event::dump (SCM self)
{
  Stream_event *ev = unsmob<Stream_event> (self);
  // Reversed alists look prettier.
  return scm_cons (scm_reverse (ev->immutable_property_alist_),
                   scm_reverse (ev->mutable_property_alist_));
}

SCM
Stream_event::undump (SCM data)
{
  Stream_event *obj = new Stream_event ();
  obj->immutable_property_alist_ = scm_reverse (scm_car (data));
  obj->mutable_property_alist_ = scm_reverse (scm_cdr (data));
  return obj->unprotect ();
}

void
warn_reassign_event_ptr (Stream_event &old_ev, Stream_event *new_ev)
{
  if (!new_ev) // not expected
    return;

  if (to_boolean (scm_equal_p (old_ev.self_scm (), new_ev->self_scm ())))
    return; // nothing of value was lost

  std::string oc = ly_symbol2string (scm_car (old_ev.get_property ("class")));
  old_ev.origin ()->warning (_f ("conflict with event: `%s'", oc.c_str ()));

  std::string nc = ly_symbol2string (scm_car (new_ev->get_property ("class")));
  new_ev->origin ()->warning (_f ("discarding event: `%s'", nc.c_str ()));
}
