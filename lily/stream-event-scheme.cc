/*
  stream-event.cc -- implement Scheme bindings for Stream_event

  source file of the GNU LilyPond music typesetter

  (c) 2006 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "stream-event.hh"

LY_DEFINE (ly_make_stream_event, "ly:make-stream-event",
	   1, 0, 0, (SCM proplist),
	   "Creates a stream event, with the given property list.\n" )
{
  SCM_ASSERT_TYPE (scm_list_p (proplist), proplist, SCM_ARG1, __FUNCTION__, "association list");
  Stream_event *e = new Stream_event (proplist);
  return e->unprotect ();
}

LY_DEFINE (ly_event_property, "ly:event-property", 
           2, 0, 0, (SCM sev, SCM sym),
	   "Get the property @var{sym} of stream event @var{mus}.\n"
	   "If @var{sym} is undefined, return @code{' ()}.\n")
{
  Stream_event *e = unsmob_stream_event (sev);
  SCM_ASSERT_TYPE (e, sev, SCM_ARG1, __FUNCTION__, "stream event");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return e->internal_get_property (sym);
}
