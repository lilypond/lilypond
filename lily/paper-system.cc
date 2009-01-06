/*
  paper-system.cc -- implement Prob functions for paper-system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-system.hh"
#include "item.hh"

Prob *
make_paper_system (SCM immutable_init)
{
  Prob *prob = new Prob (ly_symbol2scm ("paper-system"), immutable_init);
  return prob;
}

void
paper_system_set_stencil (Prob *prob, Stencil s)
{
  SCM yext = prob->get_property ("Y-extent");

  if (is_number_pair (yext))
    {
      Box b = s.extent_box ();
      b[Y_AXIS] = ly_scm2interval (yext);

      s = Stencil (b, s.expr ());
    }

  prob->set_property ("stencil", s.smobbed_copy ());
}
