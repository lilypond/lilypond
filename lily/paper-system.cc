/*
  paper-system.cc -- implement Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2005 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-system.hh"

#include "virtual-methods.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Paper_system);
IMPLEMENT_TYPE_P (Paper_system, "ly:paper-system?");
IMPLEMENT_DEFAULT_EQUAL_P (Paper_system);



Paper_system::Paper_system (Stencil s, bool is_title)
{
  is_title_ = is_title;
  number_ = 0;
  break_before_penalty_ = 0;
  smobify_self ();
  stencil_ = s;
  staff_refpoints_ = Interval(0,0);
}

Paper_system::~Paper_system ()
{
}

SCM
Paper_system::mark_smob (SCM smob)
{
  Paper_system *system = (Paper_system*) SCM_CELL_WORD_1 (smob);
  return system-> stencil_.expr ();
}

int
Paper_system::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_system *p = (Paper_system*) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<", port);
  scm_puts (classname (p), port);
  scm_puts (" ", port);
  scm_puts (to_string (p->number_).to_str0 (), port);
  scm_puts ("p ", port);
  scm_puts (to_string (p->break_before_penalty_).to_str0 (), port);
  if (p->is_title ())
    scm_puts (" t", port);
  scm_puts (" >", port);
  return 1;
}

bool
Paper_system::is_title () const
{
  return is_title_;
}

Real
Paper_system::break_before_penalty () const
{
  return break_before_penalty_;
}

Stencil
Paper_system::to_stencil () const
{
  return stencil_;
}
