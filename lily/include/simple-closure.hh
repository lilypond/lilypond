/*
  simple-closure.hh -- declare simple closures 

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SIMPLE_CLOSURE_HH
#define SIMPLE_CLOSURE_HH

#include "lily-guile.hh"

bool is_simple_closure (SCM s);
SCM simple_closure_expression (SCM smob);
SCM evaluate_with_simple_closure (SCM delayed_argument, SCM expr, bool pure, int start, int end);
SCM ly_make_simple_closure (SCM);

#endif /* SIMPLE_CLOSURE_HH */
