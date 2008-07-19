/*
  output-def-scheme.cc -- implement Output_def bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "output-def.hh"

#include "pango-font.hh"
#include "modified-font-metric.hh"
#include "ly-module.hh"
#include "context-def.hh"
#include "lily-parser.hh"

LY_DEFINE (ly_output_def_lookup, "ly:output-def-lookup",
	   2, 1, 0, (SCM pap, SCM sym, SCM def),
	   "Look up @var{sym} in the @var{pap} output definition"
	   " (e.g., @code{\\paper}).  Return the value or @var{def}"
	   " (which defaults to @code{'()}) if undefined.")
{
  LY_ASSERT_SMOB (Output_def, pap, 1);
  Output_def *op = unsmob_output_def (pap);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  SCM answer = op->lookup_variable (sym);
  if (answer == SCM_UNDEFINED)
    {
      if (def == SCM_UNDEFINED)
	def = SCM_EOL;

      answer = def;
    }

  return answer;
}

LY_DEFINE (ly_output_def_scope, "ly:output-def-scope",
	   1, 0, 0, (SCM def),
	   "Get the variable scope inside @var{def}.")
{
  LY_ASSERT_SMOB (Output_def, def, 1);
  Output_def *op = unsmob_output_def (def);
  return op->scope_;
}

LY_DEFINE (ly_output_def_parent, "ly:output-def-parent",
	   1, 0, 0, (SCM def),
	   "Get the parent output definition of @var{def}.")
{
  LY_ASSERT_SMOB (Output_def, def, 1);
  Output_def *op = unsmob_output_def (def);
  return op->parent_ ? op->parent_->self_scm () : SCM_EOL;
}

LY_DEFINE (ly_output_def_set_variable_x, "ly:output-def-set-variable!",
           3, 0, 0, (SCM def, SCM sym, SCM val),
           "Set an output definition @var{def} variable @var{sym} to @var{val}.")
{
  LY_ASSERT_SMOB (Output_def, def, 1);
  Output_def *output_def = unsmob_output_def (def);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  output_def->set_variable (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_output_def_clone, "ly:output-def-clone",
	   1, 0, 0, (SCM def),
	   "Clone output definition @var{def}.")
{
  LY_ASSERT_SMOB (Output_def, def, 1);
  Output_def *op = unsmob_output_def (def);

  Output_def *clone = op->clone ();
  return clone->unprotect ();
}

LY_DEFINE (ly_output_description, "ly:output-description",
	   1, 0, 0, (SCM output_def),
	   "Return the description of translators in @var{output-def}.")
{
  Output_def *id = unsmob_output_def (output_def);

  SCM al = ly_module_2_alist (id->scope_);
  SCM ell = SCM_EOL;
  for (SCM s = al; scm_is_pair (s); s = scm_cdr (s))
    {
      Context_def *td = unsmob_context_def (scm_cdar (s));
      SCM key = scm_caar (s);
      if (td && key == td->get_context_name ())
	ell = scm_cons (scm_cons (key, td->to_alist ()), ell);
    }
  return ell;
}

LY_DEFINE (ly_output_def_p, "ly:output-def?",
	   1, 0, 0, (SCM def),
	   "Is @var{def} a layout definition?")
{
  return ly_bool2scm (unsmob_output_def (def));
}

LY_DEFINE (ly_paper_outputscale, "ly:paper-outputscale",
	   1, 0, 0, (SCM bp),
	   "Get output-scale for @var{bp}.")
{
  LY_ASSERT_SMOB (Output_def, bp, 1);
  Output_def *b = unsmob_output_def (bp);
  return scm_from_double (output_scale (b));
}

LY_DEFINE (ly_make_output_def, "ly:make-output-def",
	   0, 0, 0, (),
	   "Make an output definition.")
{
  Output_def *bp = new Output_def;
  return bp->unprotect ();
}

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
	   (SCM paper_smob, SCM chain),
	   "Return a font metric satisfying the font-qualifiers"
	   " in the alist chain @var{chain}.  (An alist chain is a"
	   " list of alists, containing grob properties.)")
{
  LY_ASSERT_SMOB (Output_def, paper_smob, 1);

  Output_def *paper = unsmob_output_def (paper_smob);
  Font_metric *fm = select_font (paper, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
	   (SCM layout_smob, SCM name),
	   "Return the layout variable @var{name}.")
{
  LY_ASSERT_SMOB (Output_def, layout_smob, 1);
  Output_def *layout = unsmob_output_def (layout_smob);
  return scm_from_double (layout->get_dimension (name));
}

LY_DEFINE (ly_paper_fonts, "ly:paper-fonts",
	   1, 0, 0,
	   (SCM bp),
	   "Return fonts from the @code{\\paper} block @var{bp}.")
{
  LY_ASSERT_SMOB (Output_def, bp, 1);
  Output_def *b = unsmob_output_def (bp);

  SCM tab1 = b->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  SCM tab2 = b->lookup_variable (ly_symbol2scm ("pango-fonts"));

  SCM alist1 = SCM_EOL;
  if (scm_hash_table_p (tab1) == SCM_BOOL_T)
    {
      alist1 = scm_append (ly_alist_vals (ly_hash2alist (tab1)));

      alist1 = ly_alist_vals (alist1);
    }

  SCM alist2 = SCM_EOL;
  if (scm_hash_table_p (tab2) == SCM_BOOL_T)
    {
      // strip original-fonts/pango-font-descriptions
      alist2 = scm_append (ly_alist_vals (ly_hash2alist (tab2)));

      // strip size factors
      alist2 = ly_alist_vals (alist2);
    }

  SCM alist = scm_append (scm_list_2 (alist1, alist2));
  SCM font_list = SCM_EOL;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);

      Font_metric *fm = unsmob_metrics (entry);

      if (dynamic_cast<Modified_font_metric *> (fm)
	  || dynamic_cast<Pango_font *> (fm))
	font_list = scm_cons (fm->self_scm (), font_list);
    }

  return font_list;
}
