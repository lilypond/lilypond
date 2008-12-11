/*
  stencil-expression.cc -- keep track of which expressions are valid
  stencil exps.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "stencil.hh"

static SCM heads;

void register_stencil_head (SCM symbol)
{
  if (!heads)
    heads = scm_permanent_object (scm_cons (SCM_EOL, SCM_EOL));
  
  scm_set_object_property_x (symbol, ly_symbol2scm ("stencil-head?"), SCM_BOOL_T);
  scm_set_cdr_x (heads,  scm_cons (symbol, scm_cdr (heads)));
}

bool
is_stencil_head (SCM symbol)
{
  return scm_object_property (symbol, ly_symbol2scm ("stencil-head?"))
    == SCM_BOOL_T;
}

SCM
all_stencil_heads ()
{
  return scm_cdr (heads);
}

