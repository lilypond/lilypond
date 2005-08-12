/*
  stencil-expression.cc -- keep track of which expressions are valid
  stencil exps.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "stencil.hh"

#include "protected-scm.hh"

static Protected_scm heads = SCM_EOL;

void register_stencil_head (SCM symbol)
{
  scm_set_object_property_x (symbol, ly_symbol2scm ("stencil-head?"), SCM_BOOL_T);
  heads = scm_cons (symbol, heads);
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
  return heads;
}

