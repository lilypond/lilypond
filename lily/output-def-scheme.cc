/*
  output-def-scheme.cc --  implement Output_def bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "font-metric.hh"
#include "pango-font.hh"
#include "modified-font-metric.hh"
#include "output-def.hh"
#include "ly-module.hh"
#include "context-def.hh"

LY_DEFINE (ly_layout_lookup, "ly:output-def-lookup",
	   2, 0, 0, (SCM pap, SCM sym),
	   "Lookup @var{sym} in @var{pap}. "
	   "Return the value or @code{'()} if undefined.")
{
  Output_def *op = unsmob_output_def (pap);
  SCM_ASSERT_TYPE (op, pap, SCM_ARG1, __FUNCTION__, "Output_def");
  SCM_ASSERT_TYPE (scm_is_symbol (sym), sym, SCM_ARG2, __FUNCTION__, "symbol");

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
  for (SCM s = al; scm_is_pair (s); s = scm_cdr (s))
    {
      Context_def * td = unsmob_context_def (scm_cdar (s));
      SCM key = scm_caar (s);
      if (td && key == td->get_context_name ())
	ell = scm_cons (scm_cons (key, td->to_alist ()),  ell);
    }
  return ell;  
}

LY_DEFINE (ly_layout_def_p, "ly:layout-def?",
	   1, 0, 0, (SCM def),
	   "Is @var{def} a layout definition?")
{
  return ly_bool2scm (unsmob_output_def (def));
}

LY_DEFINE (ly_paper_outputscale, "ly:paper-outputscale",
	  1, 0, 0, (SCM bp),
	  "Get outputscale for BP.")
{
  Output_def *b = unsmob_output_def (bp);
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "paper");
  return scm_make_real (output_scale (b));
}

LY_DEFINE (ly_make_output_def, "ly:make-output-def",
	   0, 0, 0, (),
	   "Make a output def.")
{
  Output_def *bp = new Output_def ;
  return scm_gc_unprotect_object (bp->self_scm ());
}

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
	   (SCM paper_smob, SCM chain),

	   "Return a font metric satisfying the font-qualifiers "
	   "in the alist chain @var{chain}.\n"
	   "(An alist chain is a list of alists, "
	   "containing grob properties).\n")
{
  Output_def *paper = unsmob_output_def (paper_smob);
  SCM_ASSERT_TYPE (paper, paper_smob, SCM_ARG1,
		   __FUNCTION__, "paper definition");
  
  Font_metric *fm = select_font (paper, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
	   (SCM layout_smob, SCM name),
	   "Return the layout variable @var{name}.")
{
  Output_def *layout = unsmob_output_def (layout_smob);
  SCM_ASSERT_TYPE (layout, layout_smob, SCM_ARG1,
		   __FUNCTION__, "layout definition");
  return scm_make_real (layout->get_dimension (name));
}

LY_DEFINE (ly_paper_fonts, "ly:paper-fonts",
	   1, 0, 0,
	   (SCM bp),
	   "Return fonts from the @code{\\paper} block @var{bp}.")
{
  Output_def *b = unsmob_output_def (bp);

  SCM font_table = b->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  
  SCM_ASSERT_TYPE (b, bp, SCM_ARG1, __FUNCTION__, "paper");

  SCM ell = SCM_EOL;
  if (scm_hash_table_p (font_table) == SCM_BOOL_T)
    {
      SCM func = ly_lily_module_constant ("hash-table->alist");

      for (SCM s = scm_call_1 (func, font_table); scm_is_pair (s);
	   s = scm_cdr (s))
	{
	  SCM entry = scm_car (s);
	  for (SCM t = scm_cdr (entry); scm_is_pair (t); t = scm_cdr (t))
	    {
	      Font_metric *fm = unsmob_metrics (scm_cdar (t));

	      if (dynamic_cast<Modified_font_metric*> (fm)
		  || dynamic_cast<Pango_font*> (fm))
		ell = scm_cons (fm->self_scm (), ell);
	    }
	}
    }
  return ell;
}
