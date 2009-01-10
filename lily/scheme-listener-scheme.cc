/*
  scheme-listener-scheme.cc -- Connect listeners to Scheme through Scheme_listener

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "scheme-listener.hh"

LY_DEFINE (ly_make_listener, "ly:make-listener",
	   1, 0, 0, (SCM callback),
	   "Create a listener.  Any time the listener hears an object,"
	   " it will call @var{callback} with that object.\n"
	   "\n"
	   "@var{callback} should take exactly one argument.")
{
  LY_ASSERT_TYPE (ly_is_procedure, callback, 1);
  Scheme_listener *l = new Scheme_listener (callback);
  SCM listener = GET_LISTENER (l->call).smobbed_copy ();
  l->unprotect ();
  return listener;
}
