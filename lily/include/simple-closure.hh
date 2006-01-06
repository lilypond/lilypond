/*
  simple-closure.hh -- declare simple closures 

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#ifndef SIMPLE_CLOSURE_HH
#define SIMPLE_CLOSURE_HH

bool is_simple_closure (SCM s);
SCM simple_closure_expression (SCM smob);
SCM evaluate_with_simple_closure (SCM delayed_argument, SCM expr);
SCM ly_make_simple_closure (SCM);

#endif /* SIMPLE_CLOSURE_HH */
