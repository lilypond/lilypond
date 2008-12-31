/*
  listener.cc -- implement Listener and Listener_target

  source file of the GNU LilyPond music typesetter

  (c) 2005 Erik Sandberg  <mandolaerik@gmail.com>
*/

#include "listener.hh"
#include "ly-smobs.icc"
#include "warn.hh"

Listener::Listener ()
{
  target_ = 0;
  type_ = 0;
}

Listener::Listener (const void *target, Listener_function_table *type)
{
  target_ = (void *)target;
  type_ = type;
}

Listener::Listener (Listener const &other)
{
  target_ = other.target_;
  type_ = other.type_; 
}

void Listener::listen (SCM ev) const {
  (type_->listen_callback) (target_, ev);
}

SCM
Listener::mark_smob (SCM sm)
{
  Listener *me = (Listener *) SCM_CELL_WORD_1 (sm);
  if (me->type_)
    (me->type_->mark_callback) (me->target_);
  return SCM_EOL;
}

int
Listener::print_smob (SCM, SCM p, scm_print_state*)
{
  scm_puts ("#<Listener>", p);
  return 1;
}

SCM
Listener::equal_p (SCM a, SCM b)
{
  Listener *l1 = unsmob_listener (a);
  Listener *l2 = unsmob_listener (b);

  return (*l1 == *l2) ? SCM_BOOL_T : SCM_BOOL_F;
}

IMPLEMENT_SIMPLE_SMOBS (Listener);
IMPLEMENT_TYPE_P (Listener, "ly:listener?");
