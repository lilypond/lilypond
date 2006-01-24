/*
  paper-system.cc -- implement Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-system.hh"
#include "item.hh"

Paper_system::Paper_system (Stencil s, SCM immutable_init)
  : Prob (immutable_init)
{
  SCM yext = get_property ("Y-extent");

  if (is_number_pair (yext))
    {
      
      Box b = s.extent_box();
      b[Y_AXIS] = ly_scm2interval (yext);

      s = Stencil (b, s.expr ());
    }

  set_property ("stencil", s.smobbed_copy ());
}

Paper_system *
unsmob_paper_system (SCM x)
{
  Prob *prob = unsmob_prob (x);
  return dynamic_cast<Paper_system*> (prob);
}

LY_DEFINE(ly_paper_system_p, "ly:paper-system?",
	  1,0,0, (SCM obj),
	  "Type predicate.")
{
  return scm_from_bool (unsmob_paper_system (obj));
}
