/*
  scheme-listener.cc -- Implement Scheme_listener

  source file of the GNU LilyPond music typesetter

  (c) 2006--2008 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "scheme-listener.hh"

IMPLEMENT_LISTENER (Scheme_listener, call)
void
Scheme_listener::call (SCM ev)
{
  scm_call_1 (callback_, ev);
}

IMPLEMENT_SMOBS (Scheme_listener);
IMPLEMENT_DEFAULT_EQUAL_P (Scheme_listener);

Scheme_listener::Scheme_listener (SCM c)
{
  callback_ = SCM_EOL;
  self_scm_ = SCM_EOL;
  smobify_self ();
  callback_ = c; 
}

SCM
Scheme_listener::mark_smob (SCM obj)
{
  Scheme_listener *me = (Scheme_listener *) SCM_CELL_WORD_1 (obj);
  return me->callback_;
}

int
Scheme_listener::print_smob (SCM obj, SCM p, scm_print_state*)
{
  Scheme_listener *me = (Scheme_listener *) SCM_CELL_WORD_1 (obj);
  scm_puts ("#<Scheme_listener ", p);
  scm_write (me->callback_, p);
  scm_puts (">", p);
  return 1;
}

Scheme_listener::~Scheme_listener ()
{
}
