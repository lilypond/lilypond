/*
  music-output-def.cc -- implement Output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context-def.hh"
#include "file-path.hh"
#include "global-context.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "warn.hh"

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
  scope_= ly_make_anonymous_module (false);
  if (ly_c_module_p (s.scope_))
    ly_import_module (scope_, s.scope_);
}

Output_def::~Output_def ()
{
}

#include "ly-smobs.icc"
IMPLEMENT_SMOBS (Output_def);
IMPLEMENT_DEFAULT_EQUAL_P (Output_def);

SCM
Output_def::mark_smob (SCM m)
{
  Output_def *mo = (Output_def*) SCM_CELL_WORD_1 (m);

  /* FIXME: why is this necessary?
     all bookpaper_ should be protected by themselves. */
  if (mo->parent_)
    scm_gc_mark (mo->parent_->self_scm ());

  mo->derived_mark ();
  return mo->scope_;
}

void
Output_def::derived_mark ()
{
}

void
assign_context_def (Output_def * m, SCM transdef)
{
  Context_def *tp = unsmob_context_def (transdef);
  assert (tp);

  if (tp)
    {
      SCM sym = tp->get_context_name ();
      scm_module_define (m->scope_, sym, transdef);
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
  scm_puts (classname (def), p);
  
  (void)def;
  scm_puts (">", p);
  return 1;
}

Real
Output_def::get_dimension (SCM s) const
{
  SCM val = lookup_variable (s);
  return ly_scm2double (val);
}

SCM
Output_def::lookup_variable (SCM sym) const
{
  SCM var = ly_module_lookup (scope_, sym);
  if (SCM_VARIABLEP (var) && SCM_VARIABLE_REF(var) != SCM_UNDEFINED)
    return SCM_VARIABLE_REF (var);
  
  if (parent_)
    return parent_->lookup_variable (sym);
  
  return SCM_EOL;
}

SCM
Output_def::c_variable (String s) const
{
  return lookup_variable (ly_symbol2scm (s.to_str0 ()));
}

void
Output_def::set_variable (SCM sym, SCM val)
{
  scm_module_define (scope_, sym, val);
}

LY_DEFINE (ly_paper_lookup, "ly:output-def-lookup",
	   2, 0, 0, (SCM pap, SCM sym),
	   "Lookup @var{sym} in @var{pap}. "
	   "Return the value or @code{'()} if undefined.")
{
  Output_def *op = unsmob_output_def (pap);
  SCM_ASSERT_TYPE (op, pap, SCM_ARG1, __FUNCTION__, "Output_def");
  SCM_ASSERT_TYPE (ly_c_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  return op->lookup_variable (sym);
}

LY_DEFINE (ly_output_def_scope, "ly:output-def-scope",
	   1, 0, 0, (SCM def),
	   "Get the variable scope inside @var{def}.")
{
  Output_def *op = unsmob_output_def (def);
  SCM_ASSERT_TYPE (op, def, SCM_ARG1, __FUNCTION__, "Output definition");
  return op->scope_;
}

LY_DEFINE (ly_output_def_parent, "ly:output-def-parent",
	   1, 0, 0, (SCM def),
	   "Get the parent output-def of @var{def}.")
{
  Output_def *op = unsmob_output_def (def);
  SCM_ASSERT_TYPE (op, def, SCM_ARG1, __FUNCTION__, "Output definition");
  return op->parent_ ? op->parent_->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_output_def_clone, "ly:output-def-clone",
	   1, 0, 0, (SCM def),
	   "Clone @var{def}.")
{
  Output_def *op = unsmob_output_def (def);
  SCM_ASSERT_TYPE (op, def, SCM_ARG1, __FUNCTION__, "Output definition");
  SCM s = op->clone ()->self_scm ();
  scm_gc_unprotect_object (s);
  return s;
}

LY_DEFINE (ly_output_description, "ly:output-description",
	   1, 0, 0, (SCM output_def),
	   "Return the description of translators in @var{output-def}.")
{
  Output_def *id = unsmob_output_def (output_def);
  
  SCM al = ly_module2alist (id->scope_);

  SCM ell = SCM_EOL;
  for (SCM s = al; ly_c_pair_p (s); s = ly_cdr (s))
    {
      Context_def * td = unsmob_context_def (ly_cdar (s));
      SCM key = ly_caar (s);
      if (td && key == td->get_context_name ())
	ell = scm_cons (scm_cons (key, td->to_alist ()),  ell);
    }
  return ell;  
}
  
/* FIXME.  This is broken until we have a generic way of
   putting lists inside the \paper block.  */
Interval
line_dimensions_int (Output_def *def, int n)
{
  Real lw = def->get_dimension (ly_symbol2scm ("linewidth"));
  Real ind = n ? 0.0 : def->get_dimension (ly_symbol2scm ("indent"));
  return Interval (ind, lw);
}

LY_DEFINE (ly_paper_def_p, "ly:paper-def?",
	   1, 0, 0, (SCM def),
	   "Is @var{def} a paper definition?")
{
  return ly_bool2scm (unsmob_output_def (def));
}

LY_DEFINE (ly_bookpaper_outputscale, "ly:bookpaper-outputscale",
	  1, 0, 0, (SCM bp),
	  "Get outputscale for BP.")
{
  Output_def *b = unsmob_output_def (bp);
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "bookpaper");
  return scm_make_real (output_scale (b));
}

LY_DEFINE (ly_make_output_def, "ly:make-output-def",
	   0, 0, 0, (),
	   "Make a output def.")
{
  Output_def *bp = new Output_def ;
  return scm_gc_unprotect_object (bp->self_scm ());
}


