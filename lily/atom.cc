/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

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
  func_ = s;
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
