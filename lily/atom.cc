/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>
#include "ly-smobs.icc"

#include "atom.hh"
#include "interval.hh"
#include "string.hh"
#include "array.hh"
#include "debug.hh"
#include "dimensions.hh"
#include "lookup.hh"
#include "main.hh"
#include "global-ctor.hh"
#include "font-metric.hh"

Atom::Atom(SCM s)
{
  SCM onstack = s;		// protection.
  func_ = scm_protect_object (s);
  self_scm_ = SCM_EOL;
  smobify_self ();
}

SCM
Atom::mark_smob (SCM s)
{
  Atom*  a = SMOB_TO_TYPE(Atom, s);
  assert (s == a->self_scm_);
  return a->func_;
}

void
Atom::fontify (Font_metric * met)
{
  SCM desc = ly_quote_scm (met->description ());
  SCM font_switch = gh_list (ly_symbol2scm ("select-font"),
			     desc,
			     SCM_UNDEFINED);

  SCM f =func_;
  func_ = gh_list (ly_symbol2scm ("string-append"),
		   font_switch , f,
		   SCM_UNDEFINED);
}

void
Atom::do_smobify_self ()
{
}

Atom::Atom (Atom const &s)
{
  off_ = s.off_;
  func_ = s.func_;
  self_scm_= SCM_EOL;
  smobify_self ();
}
int
Atom::print_smob (SCM s, SCM p, scm_print_state*)
{
  Atom * a = unsmob_atom (s);

  scm_puts ("#<Atom off ",p);
  String str(a->off_.str ());
  scm_puts ((char *)str.ch_C(), p);
  scm_display (a->func_, p);
  scm_puts ("> ",p);  
  return 1;
}
  
IMPLEMENT_UNSMOB(Atom, atom)
IMPLEMENT_SMOBS(Atom)

