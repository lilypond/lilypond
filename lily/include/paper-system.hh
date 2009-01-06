/*
  paper-system.hh -- declare Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009  Jan Nieuwenhuizen <janneke@gnu.org>
*/
#ifndef PAPER_SYSTEM_HH
#define PAPER_SYSTEM_HH

#include "prob.hh"

/*
  A formatted "system" (A block of titling also is a Paper_system)

  To save memory, we don't keep around the System grobs, but put the
  formatted content of the grob is put into a
  Paper_system. Page-breaking handles Paper_system objects.
*/
Prob *make_paper_system (SCM immutable_init);
void paper_system_set_stencil (Prob *prob, Stencil s);

#endif /* PAPER_SYSTEM_HH */
