/*
  dispatcher.cc -- implement Scheme bindings for Dispatcher

  source file of the GNU LilyPond music typesetter

  (c) 2006--2007 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "dispatcher.hh"

LY_DEFINE (ly_make_dispatcher, "ly:make-dispatcher",
	   0, 0, 0, (),
	   "Returns a newly created dispatcher.")
{
  return (new Dispatcher ())->unprotect ();
}

LY_DEFINE (ly_register_dispatcher, "ly:connect-dispatchers",
	   2, 0, 0, (SCM to, SCM from),
	   "Makes the dispatcher @var{to} listen to events from @var{from}." )
{
  Dispatcher *t = unsmob_dispatcher (to);
  Dispatcher *f = unsmob_dispatcher (from);
  SCM_ASSERT_TYPE (t, from, SCM_ARG1, __FUNCTION__, "dispatcher");
  SCM_ASSERT_TYPE (f, to, SCM_ARG2, __FUNCTION__, "dispatcher");
  t->register_as_listener (f);

  return SCM_UNDEFINED;
}

LY_DEFINE (ly_add_listener, "ly:add-listener",
	   2, 0, 1, (SCM list, SCM disp, SCM cl),
	   "Adds the listener @var{list} to the dispatcher @var{disp}.\n"
	   " Whenever @var{disp} hears an event of class @var{cl}, it will be forwarded to @var{list}.\n" )
{
  Listener *l = unsmob_listener (list);
  Dispatcher *d = unsmob_dispatcher (disp);
  SCM_ASSERT_TYPE (l, list, SCM_ARG1, __FUNCTION__, "listener");
  SCM_ASSERT_TYPE (d, disp, SCM_ARG2, __FUNCTION__, "dispatcher");
  
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
	   "Sends the stream event @var{ev} to the dispatcher\n"
	   "@var{disp}.")
{
  Dispatcher *d = unsmob_dispatcher (disp);
  Stream_event *e = unsmob_stream_event (ev);
  SCM_ASSERT_TYPE (d, disp, SCM_ARG1, __FUNCTION__, "dispatcher");
  SCM_ASSERT_TYPE (e, ev, SCM_ARG2, __FUNCTION__, "stream event");
  d->broadcast (e);
  return SCM_UNDEFINED;
}
