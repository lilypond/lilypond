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
  translator_tab_ = new Scheme_hash_table;
  scope_ = SCM_EOL;
  smobify_self ();

  scm_gc_unprotect_object (translator_tab_->self_scm ());
  scope_ = ly_make_anonymous_module ();
}

Music_output_def::~Music_output_def ()
{
}

Music_output_def::Music_output_def (Music_output_def const &s)
{
  scope_ = SCM_EOL;
  translator_tab_ = 0;
  smobify_self ();

  translator_tab_ =   new Scheme_hash_table (*s.translator_tab_);  
  scm_gc_unprotect_object (translator_tab_->self_scm ());  
  
  scope_= ly_make_anonymous_module ();
  if (is_module (s.scope_))
    ly_import_module (scope_, s.scope_);
}


IMPLEMENT_SMOBS (Music_output_def);
IMPLEMENT_DEFAULT_EQUAL_P (Music_output_def);

SCM
Music_output_def::mark_smob (SCM m)
{
  Music_output_def * mo = (Music_output_def*) SCM_CELL_WORD_1 (m);
  if (mo->translator_tab_)
    scm_gc_mark (mo->translator_tab_->self_scm ());

  mo->derived_mark ();
  
  return mo->scope_;
}

void
Music_output_def::derived_mark ()
{
  
}

void
Music_output_def::assign_context_def (SCM transdef)
{
  Context_def *tp = unsmob_context_def (transdef);
  assert (tp);

  translator_tab_->set (tp->get_context_name (), transdef);
  
  String nm = ly_symbol2string (tp->get_context_name ()) + "Context";


  scm_module_define (scope_, ly_symbol2scm (nm.to_str0 ()), transdef);
}

/*
  find the translator for NAME. NAME must be a symbol.
*/
SCM
Music_output_def::find_context_def (SCM name) const
{  
  SCM val  =SCM_EOL;
  translator_tab_->try_retrieve (name, &val);
  return val;
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
Music_output_def::get_scmvar (String s) const
{
  return lookup_variable (ly_symbol2scm (s.to_str0 ()));
}

void
Music_output_def::set_variable (SCM sym, SCM val)
{
  scm_module_define (scope_, sym, val);
}

SCM
Music_output_def::lookup_variable (SCM sym) const
{
  SCM var = ly_module_lookup (scope_, sym);

  return scm_variable_ref (var);
}

LY_DEFINE (ly_paper_lookup, "ly:paper-lookup",
	   2, 0,0, (SCM pap, SCM sym),
	   "Lookup @var{sym} in @var{pap}. "
	   "Return the value or @code{'()} if undefined.")
{
  Music_output_def *op = unsmob_music_output_def (pap);
  SCM_ASSERT_TYPE (op, pap, SCM_ARG1, __FUNCTION__, "Paper");
  SCM_ASSERT_TYPE (is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

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
  SCM al = id->translator_tab_->to_alist ();
  SCM l = SCM_EOL;
  for (SCM s = al ; is_pair (s); s = ly_cdr (s))
    {
      Context_def * td = unsmob_context_def (ly_cdar (s));
      l = scm_cons (scm_cons (ly_caar (s), td->to_alist ()),  l);
    }
  return l;  
}
  

