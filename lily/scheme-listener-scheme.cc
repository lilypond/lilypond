/*
  scheme-listener-scheme.cc -- Connect listeners to Scheme through Scheme_listener

  source file of the GNU LilyPond music typesetter

  (c) 2006 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "scheme-listener.hh"

LY_DEFINE (ly_make_listener, "ly:make-listener",
	   1, 0, 0, (SCM callback),
	   "Creates a listener. Any time the listener hears\n"
	   " an object, it will call @var{callback}\n"
	   " with that object.\n"
	   "\n"
	   " @var{callback} should take exactly one argument." )
{
  SCM_ASSERT_TYPE (scm_procedure_p (callback), callback, SCM_ARG1, __FUNCTION__, "procedure");
  Scheme_listener *l = new Scheme_listener (callback);
  SCM listener = GET_LISTENER (l->call).smobbed_copy ();
  l->unprotect ();
  return listener;
}
