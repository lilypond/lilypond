/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2010 Erik Sandberg  <mandolaerik@gmail.com>

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

LY_DEFINE (ly_make_dispatcher, "ly:make-dispatcher",
	   0, 0, 0, (),
	   "Return a newly created dispatcher.")
{
  return (new Dispatcher ())->unprotect ();
}

LY_DEFINE (ly_connect_dispatchers, "ly:connect-dispatchers",
	   2, 0, 0, (SCM to, SCM from),
	   "Make the dispatcher @var{to} listen to events from @var{from}.")
{
  Dispatcher *t = unsmob_dispatcher (to);
  Dispatcher *f = unsmob_dispatcher (from);
  
  LY_ASSERT_SMOB (Dispatcher, to, 1); 
  LY_ASSERT_SMOB (Dispatcher, from,  2); 

  t->register_as_listener (f);

  return SCM_UNDEFINED;
}

LY_DEFINE (ly_add_listener, "ly:add-listener",
	   2, 0, 1, (SCM list, SCM disp, SCM cl),
	   "Add the listener @var{list} to the dispatcher @var{disp}."
	   "  Whenever @var{disp} hears an event of class @var{cl},"
	   " it is forwarded to @var{list}.")
{
  Listener *l = unsmob_listener (list);
  Dispatcher *d = unsmob_dispatcher (disp);

  LY_ASSERT_SMOB (Listener, list, 1); 
  LY_ASSERT_SMOB (Dispatcher, disp, 2); 
  
  for (int arg = SCM_ARG3; scm_is_pair (cl); cl = scm_cdr (cl), arg++)
    {
      SCM sym = scm_car (cl);
      SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, arg, __FUNCTION__, "symbol");
      d->add_listener (*l, sym);
    }

  return SCM_UNDEFINED;
}

LY_DEFINE (ly_broadcast, "ly:broadcast",
	   2, 0, 0, (SCM disp, SCM ev),
	   "Send the stream event @var{ev} to the dispatcher @var{disp}.")
{
  Dispatcher *d = unsmob_dispatcher (disp);
  Stream_event *e = unsmob_stream_event (ev);
 
  LY_ASSERT_SMOB (Dispatcher, disp, 1);

  LY_ASSERT_SMOB (Stream_event, ev, 2); 
  d->broadcast (e);
  return SCM_UNDEFINED;
}
