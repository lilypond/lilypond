/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Erik Sandberg  <mandolaerik@gmail.com>

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

#include "dispatcher.hh"

LY_DEFINE (ly_make_dispatcher, "ly:make-dispatcher", 0, 0, 0, (),
           "Return a newly created dispatcher.")
{
  return (new Dispatcher ())->unprotect ();
}

LY_DEFINE (ly_connect_dispatchers, "ly:connect-dispatchers", 2, 0, 0,
           (SCM to, SCM from),
           "Make the dispatcher @var{to} listen to events from @var{from}.")
{
  Dispatcher *t = unsmob<Dispatcher> (to);
  Dispatcher *f = unsmob<Dispatcher> (from);

  LY_ASSERT_SMOB (Dispatcher, to, 1);
  LY_ASSERT_SMOB (Dispatcher, from, 2);

  t->register_as_listener (f);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_disconnect_dispatchers, "ly:disconnect-dispatchers", 2, 0, 0,
           (SCM to, SCM from),
           "Stop the dispatcher @var{to} listening to events from @var{from}.")
{
  Dispatcher *t = unsmob<Dispatcher> (to);
  Dispatcher *f = unsmob<Dispatcher> (from);

  LY_ASSERT_SMOB (Dispatcher, to, 1);
  LY_ASSERT_SMOB (Dispatcher, from, 2);

  t->unregister_as_listener (f);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_add_listener, "ly:add-listener", 2, 0, 1,
           (SCM callback, SCM disp, SCM cl),
           "Add the single-argument procedure @var{callback} as listener"
           " to the dispatcher @var{disp}.  Whenever @var{disp} hears"
           " an event of class @var{cl}, it calls @var{callback} with it.")
{
  Dispatcher *d = unsmob<Dispatcher> (disp);

  LY_ASSERT_TYPE (ly_is_procedure, callback, 1);
  LY_ASSERT_SMOB (Dispatcher, disp, 2);

  for (int arg = SCM_ARG3; scm_is_pair (cl); cl = scm_cdr (cl), arg++)
    {
      SCM sym = scm_car (cl);
      SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, arg, __FUNCTION__, "symbol");
      d->add_listener (callback, sym);
    }

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_listened_event_types, "ly:listened-event-types", 1, 0, 0,
           (SCM disp),
           "Return a list of all event types that @var{disp} listens"
           " to.")
{
  LY_ASSERT_SMOB (Dispatcher, disp, 1);

  SCM result = unsmob<Dispatcher> (disp)->listened_types ();

  scm_remember_upto_here_1 (disp);

  return result;
}

LY_DEFINE (ly_listened_event_class_p, "ly:listened-event-class?", 2, 0, 0,
           (SCM disp, SCM cl),
           "Does @var{disp} listen to any event type in the list"
           " @var{cl}?")
{
  LY_ASSERT_SMOB (Dispatcher, disp, 1);
  LY_ASSERT_TYPE (scm_is_pair, cl, 2);

  bool result = unsmob<Dispatcher> (disp)->is_listened_class (cl);

  scm_remember_upto_here_1 (disp);

  return scm_from_bool (result);
}

LY_DEFINE (ly_broadcast, "ly:broadcast", 2, 0, 0, (SCM disp, SCM ev),
           "Send the stream event @var{ev} to the dispatcher @var{disp}.")
{
  Dispatcher *d = unsmob<Dispatcher> (disp);
  Stream_event *e = unsmob<Stream_event> (ev);

  LY_ASSERT_SMOB (Dispatcher, disp, 1);

  LY_ASSERT_SMOB (Stream_event, ev, 2);
  d->broadcast (e);
  return SCM_UNSPECIFIED;
}
