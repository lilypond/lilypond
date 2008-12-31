/*
  music-output-def.cc -- implement Output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "output-def.hh"

#include "context-def.hh"
#include "file-path.hh"
#include "global-context.hh"
#include "interval.hh"
#include "main.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "warn.hh"

#include "ly-smobs.icc"

#include "program-option.hh"

#include "string-convert.hh"

Output_def::Output_def ()
{
  scope_ = SCM_EOL;
  parent_ = 0;

  smobify_self ();
  
  scope_ = ly_make_anonymous_module (false);
}

Output_def::Output_def (Output_def const &s)
{
  scope_ = SCM_EOL;
  parent_ = 0;
  smobify_self ();

  input_origin_ = s.input_origin_;
  scope_ = ly_make_anonymous_module (false);
  if (ly_is_module (s.scope_))
    ly_module_copy (scope_, s.scope_);
}

Output_def::~Output_def ()
{
}

IMPLEMENT_SMOBS (Output_def);
IMPLEMENT_DEFAULT_EQUAL_P (Output_def);

SCM
Output_def::mark_smob (SCM m)
{
  Output_def *mo = (Output_def*) SCM_CELL_WORD_1 (m);

  /* FIXME: why is this necessary?
     all paper_ should be protected by themselves. */
  if (mo->parent_)
    scm_gc_mark (mo->parent_->self_scm ());

  return mo->scope_;
}

void
assign_context_def (Output_def * m, SCM transdef)
{
  Context_def *tp = unsmob_context_def (transdef);
  assert (tp);

  if (tp)
    {
      SCM sym = tp->get_context_name ();
      m->set_variable (sym, transdef);
    }  
}

/* find the translator for NAME. NAME must be a symbol. */
SCM
find_context_def (Output_def const *m, SCM name)
{
  Context_def *cd = unsmob_context_def (m->lookup_variable (name));
  return cd ? cd->self_scm () : SCM_EOL;
}

int
Output_def::print_smob (SCM s, SCM p, scm_print_state *)
{
  Output_def * def = unsmob_output_def (s);
  scm_puts ("#< ", p);
  scm_puts (def->class_name (), p);
  scm_puts (">", p);
  return 1;
}

Real
Output_def::get_dimension (SCM s) const
{
  SCM val = lookup_variable (s);
  return scm_to_double (val);
}

SCM
Output_def::lookup_variable (SCM sym) const
{
  SCM var = ly_module_lookup (scope_, sym);
  if (SCM_VARIABLEP (var) && SCM_VARIABLE_REF (var) != SCM_UNDEFINED)
    return SCM_VARIABLE_REF (var);
  
  if (parent_)
    return parent_->lookup_variable (sym);
  
  return SCM_UNDEFINED;
}

SCM
Output_def::c_variable (string s) const
{
  return lookup_variable (ly_symbol2scm (s.c_str ()));
}

void
Output_def::set_variable (SCM sym, SCM val)
{
  scm_module_define (scope_, sym, val);
}

  
/* FIXME.  This is broken until we have a generic way of
   putting lists inside the \layout block.  */
Interval
line_dimensions_int (Output_def *def, int n)
{
  Real lw = def->get_dimension (ly_symbol2scm ("line-width"));
  Real ind = n
    ? def->get_dimension (ly_symbol2scm ("short-indent"))
    : def->get_dimension (ly_symbol2scm ("indent"));
  return Interval (ind, lw);
}


