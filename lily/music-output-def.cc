/*
  music-output-def.cc -- implement Music_output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "scm-hash.hh"
#include "context-def.hh"
#include "file-path.hh"
#include "global-context.hh"
#include "lily-guile.hh"
#include "ly-module.hh"
#include "main.hh"
#include "music-output-def.hh"
#include "paper-def.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Music_output_def::Music_output_def ()
{
  scope_ = SCM_EOL;
  smobify_self ();

  scope_ = ly_make_anonymous_module (false);
}

Music_output_def::~Music_output_def ()
{
}

Music_output_def::Music_output_def (Music_output_def const &s)
{
  scope_ = SCM_EOL;
  smobify_self ();

  scope_= ly_make_anonymous_module (false);
  if (is_module (s.scope_))
    ly_import_module (scope_, s.scope_);
}


IMPLEMENT_SMOBS (Music_output_def);
IMPLEMENT_DEFAULT_EQUAL_P (Music_output_def);

SCM
Music_output_def::mark_smob (SCM m)
{
  Music_output_def * mo = (Music_output_def*) SCM_CELL_WORD_1 (m);
  return mo->scope_;
}

void
Music_output_def::assign_context_def (SCM transdef)
{
  Context_def *tp = unsmob_context_def (transdef);
  assert (tp);

  if (tp)
    {
      SCM sym = tp->get_context_name ();
      scm_module_define (scope_, sym, transdef);
    }  
  
  String nm = ly_symbol2string (tp->get_context_name ()) + "Context";
  scm_module_define (scope_, ly_symbol2scm (nm.to_str0 ()), transdef);
}

/*
  find the translator for NAME. NAME must be a symbol.
*/
SCM
Music_output_def::find_context_def (SCM name) const
{  
  SCM var = ly_module_lookup (scope_, name);

  if (var != SCM_BOOL_F)
    {
      var = scm_variable_ref (var);
      Context_def *cd = (unsmob_context_def (var));
      return cd ? cd->self_scm () : SCM_EOL;
    }
  else
    return SCM_EOL;
}

int
Music_output_def::print_smob (SCM s, SCM p, scm_print_state *)
{
  Music_output_def * def = unsmob_music_output_def (s);
  scm_puts ("#< ", p);
  scm_puts (classname (def), p);
  
  (void)def;
  scm_puts (">", p);
  return 1;
}

SCM
Music_output_def::lookup_variable (SCM sym) const
{
  SCM var = ly_module_lookup (scope_, sym);

  return scm_variable_ref (var);
}

SCM
Music_output_def::c_variable (String s) const
{
  return lookup_variable (ly_symbol2scm (s.to_str0 ()));
}

void
Music_output_def::set_variable (SCM sym, SCM val)
{
  scm_module_define (scope_, sym, val);
}

LY_DEFINE (ly_paper_lookup, "ly:paper-lookup",
	   2, 0,0, (SCM pap, SCM sym),
	   "Lookup @var{sym} in @var{pap}. "
	   "Return the value or @code{'()} if undefined.")
{
  Music_output_def *op = unsmob_music_output_def (pap);
  SCM_ASSERT_TYPE (op, pap, SCM_ARG1, __FUNCTION__, "Paper");
  SCM_ASSERT_TYPE (ly_c_symbol_p (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

  SCM var = ly_module_lookup (op->scope_, sym);
  if (SCM_VARIABLEP (var))
    return SCM_VARIABLE_REF (var);
  else
    return SCM_EOL;
}

LY_DEFINE (ly_output_def_scope, "ly:output-def-scope",
	   1, 0,0, (SCM def),
	   "Get the variable scope inside @var{def}.")
{
  Music_output_def *op = unsmob_music_output_def (def);
  SCM_ASSERT_TYPE (op, def, SCM_ARG1, __FUNCTION__, "Output definition");
  return op->scope_;
}

LY_DEFINE (ly_output_def_clone, "ly:output-def-clone",
	   1, 0, 0, (SCM def),
	   "Clone @var{def}.")
{
  Music_output_def *op = unsmob_music_output_def (def);
  SCM_ASSERT_TYPE (op, def, SCM_ARG1, __FUNCTION__, "Output definition");
  SCM s = op->clone ()->self_scm ();
  scm_gc_unprotect_object (s);
  return s;
}

LY_DEFINE(ly_output_description, "ly:output-description",
	  1,0,0,
	  (SCM output_def),
	  "Return the description of translators in @var{output-def}.")
{
  Music_output_def *id = unsmob_music_output_def (output_def);
  
  SCM al =ly_module_to_alist (id->scope_);

  SCM l = SCM_EOL;
  for (SCM s = al ; ly_c_pair_p (s); s = ly_cdr (s))
    {
      Context_def * td = unsmob_context_def (ly_cdar (s));
      SCM key = ly_caar (s);
      if (td && key == td->get_context_name ())
	{
	  
	  l = scm_cons (scm_cons (key, td->to_alist ()),  l);
	}
    }
  return l;  
}
  

