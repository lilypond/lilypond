/*
  atom.cc -- implement Atom

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>

#include "atom.hh"
#include "lookup.hh"
#include "global-ctor.hh"
#include "font-metric.hh"


#if 0
SCM translate_sym;

static void init()
{
  translate_sym = sly_symbol2scm ("translate-atom");
}

ADD_SCM_INIT_FUNC(atom, init);
#endif

SCM
translate_atom (Offset o, SCM func)
{
  return gh_list (ly_symbol2scm ("translate-atom"),
		  ly_quote_scm (to_scm (o)),
		  func,
		  SCM_UNDEFINED);
}

SCM
translate_atom_axis (Real r, Axis a, SCM func)
{
  //  off_[a] += r;
  Offset o ;
  o[a] = r;
  return gh_list (ly_symbol2scm ("translate-atom"),
		  ly_quote_scm (to_scm (o)),
		  func,
		  SCM_UNDEFINED);
}


SCM
fontify_atom(Font_metric * met, SCM f)
{
  return  gh_list (ly_symbol2scm ("fontify"),
		   ly_quote_scm (met->description ()), f, SCM_UNDEFINED);
}
