/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005-2006 Erik Sandberg  <mandolaerik@gmail.com>

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

#include "ly-smobs.icc"
#include "context.hh"
#include "input.hh"
#include "input.hh"

/* TODO: Rename Stream_event -> Event */

Stream_event::Stream_event ()
  : Prob (ly_symbol2scm ("Stream_event"), SCM_EOL)
{
}

Stream_event::Stream_event (SCM event_class, SCM mutable_props)
  : Prob (ly_symbol2scm ("Stream_event"),
	  scm_list_1 (scm_cons (ly_symbol2scm ("class"), event_class)))
{
  mutable_property_alist_ = mutable_props;
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
  Input *i = unsmob_input (get_property ("origin"));
  return i ? i : &dummy_input_global;
}

void
Stream_event::set_spot (Input *i)
{
  set_property ("origin", make_input (*i));
}

bool
Stream_event::internal_in_event_class (SCM class_name)
{
  SCM cl = get_property ("class");
  cl = scm_call_1 (ly_lily_module_constant ("ly:make-event-class"), cl);
  return scm_c_memq (class_name, cl) != SCM_BOOL_F;
}

IMPLEMENT_TYPE_P (Stream_event, "ly:stream-event?");

MAKE_SCHEME_CALLBACK (Stream_event, undump, 1);
MAKE_SCHEME_CALLBACK (Stream_event, dump, 1);

SCM
Stream_event::dump (SCM self)
{
  Stream_event *ev = unsmob_stream_event (self);
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

Stream_event *
unsmob_stream_event (SCM m)
{
  return dynamic_cast<Stream_event*> (unsmob_prob (m));
}
