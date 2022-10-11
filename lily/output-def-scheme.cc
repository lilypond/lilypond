/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "output-def.hh"

#include "pango-font.hh"
#include "modified-font-metric.hh"
#include "ly-module.hh"
#include "context-def.hh"
#include "lily-parser.hh"

LY_DEFINE (ly_output_def_lookup, "ly:output-def-lookup", 2, 1, 0,
           (SCM def, SCM sym, SCM val),
           R"(
Return the value of @var{sym} in output definition @var{def} (e.g.,
@code{\paper}).  If no value is found, return @var{val} or @code{'()} if
@var{val} is undefined.
           )")
{
  auto *const op = LY_ASSERT_SMOB (Output_def, def, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);

  SCM answer = op->lookup_variable (sym);
  if (SCM_UNBNDP (answer))
    {
      if (SCM_UNBNDP (val))
        val = SCM_EOL;

      answer = val;
    }

  return answer;
}

LY_DEFINE (ly_output_def_scope, "ly:output-def-scope", 1, 0, 0, (SCM def),
           R"(
Return the variable scope inside @var{def}.
           )")
{
  auto *const op = LY_ASSERT_SMOB (Output_def, def, 1);
  return op->scope_;
}

LY_DEFINE (ly_output_def_parent, "ly:output-def-parent", 1, 1, 0,
           (SCM output_def, SCM default_value),
           R"(
Return the parent output definition of @var{output-def}, or
@var{default-value} if @var{output-def} has no parent.
@var{default-value} is optional, and defaults to @code{'()}.
           )")
{
  auto *const op = LY_ASSERT_SMOB (Output_def, output_def, 1);
  if (op->parent_)
    return op->parent_->self_scm ();
  if (SCM_UNBNDP (default_value))
    return SCM_EOL;
  return default_value;
}

LY_DEFINE (ly_output_def_set_variable_x, "ly:output-def-set-variable!", 3, 0, 0,
           (SCM def, SCM sym, SCM val),
           R"(
Set an output definition @var{def} variable @var{sym} to @var{val}.
           )")
{
  auto *const output_def = LY_ASSERT_SMOB (Output_def, def, 1);
  LY_ASSERT_TYPE (ly_is_symbol, sym, 2);
  output_def->set_variable (sym, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_output_def_clone, "ly:output-def-clone", 1, 0, 0, (SCM def),
           R"(
Clone output definition @var{def}.
           )")
{
  auto *const op = LY_ASSERT_SMOB (Output_def, def, 1);

  Output_def *clone = op->clone ();
  return clone->unprotect ();
}

LY_DEFINE (ly_output_description, "ly:output-description", 1, 0, 0,
           (SCM output_def),
           R"(
Return the description of translators in @var{output-def}.
           )")
{
  auto *const id = LY_ASSERT_SMOB (Output_def, output_def, 1);

  SCM al = ly_module_2_alist (id->scope_);
  SCM ell = SCM_EOL;
  for (SCM s = al; scm_is_pair (s); s = scm_cdr (s))
    {
      Context_def *td = unsmob<Context_def> (scm_cdar (s));
      SCM key = scm_caar (s);
      if (td && scm_is_eq (key, td->get_context_name ()))
        ell = scm_cons (scm_cons (key, td->to_alist ()), ell);
    }
  return ell;
}

LY_DEFINE (ly_output_find_context_def, "ly:output-find-context-def", 1, 1, 0,
           (SCM output_def, SCM context_name),
           R"(
Return an alist of all context defs (matching @var{context-name} if given) in
@var{output-def}.
           )")
{
  auto *const id = LY_ASSERT_SMOB (Output_def, output_def, 1);
  if (!SCM_UNBNDP (context_name))
    LY_ASSERT_TYPE (ly_is_symbol, context_name, 2);

  SCM al = ly_module_2_alist (id->scope_);
  SCM ell = SCM_EOL;
  for (SCM s = al; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM p = scm_car (s);
      Context_def *td = unsmob<Context_def> (scm_cdr (p));
      if (td && scm_is_eq (scm_car (p), td->get_context_name ())
          && (SCM_UNBNDP (context_name) || td->is_alias (context_name)))
        ell = scm_cons (p, ell);
    }
  return ell;
}

const char *const Output_def::type_p_name_ = "ly:output-def?";

LY_DEFINE (ly_paper_outputscale, "ly:paper-outputscale", 1, 0, 0, (SCM def),
           R"(
Return the @code{output-scale} for output definition @var{def}.
           )")
{
  auto *const b = LY_ASSERT_SMOB (Output_def, def, 1);
  return to_scm (output_scale (b));
}

LY_DEFINE (ly_make_output_def, "ly:make-output-def", 0, 0, 0, (),
           R"(
Make an output definition.
           )")
{
  Output_def *bp = new Output_def;
  return bp->unprotect ();
}

LY_DEFINE (ly_paper_get_font, "ly:paper-get-font", 2, 0, 0,
           (SCM def, SCM chain),
           R"(
Find a font metric in output definition @var{def} satisfying the font
qualifiers in alist chain @var{chain}, and return it.  (An alist chain is a
list of alists, containing grob properties.)
           )")
{
  auto *const paper = LY_ASSERT_SMOB (Output_def, def, 1);

  Font_metric *fm = select_font (paper, chain);
  return fm->self_scm ();
}

LY_DEFINE (ly_paper_get_number, "ly:paper-get-number", 2, 0, 0,
           (SCM def, SCM sym),
           R"(
Return the value of variable @var{sym} in output definition @var{def} as a
double.
           )")
{
  auto *const layout = LY_ASSERT_SMOB (Output_def, def, 1);
  return to_scm (layout->get_dimension (sym));
}

LY_DEFINE (ly_paper_fonts, "ly:paper-fonts", 1, 0, 0, (SCM def),
           R"(
Return a list containing the fonts from output definition @var{def} (e.g.,
@code{\paper}).
           )")
{
  auto *const b = LY_ASSERT_SMOB (Output_def, def, 1);

  SCM tab1 = b->lookup_variable (ly_symbol2scm ("scaled-fonts"));
  SCM tab2 = b->lookup_variable (ly_symbol2scm ("pango-fonts"));

  SCM alist1 = SCM_EOL;
  if (from_scm<bool> (scm_hash_table_p (tab1)))
    {
      alist1 = scm_append (ly_alist_vals (ly_hash2alist (tab1)));

      alist1 = ly_alist_vals (alist1);
    }

  SCM alist2 = SCM_EOL;
  if (scm_is_true (scm_hash_table_p (tab2)))
    {
      // strip original-fonts/pango-font-descriptions
      alist2 = scm_append (ly_alist_vals (ly_hash2alist (tab2)));

      // strip size factors
      alist2 = ly_alist_vals (alist2);
    }

  SCM alist = ly_append (alist1, alist2);
  SCM font_list = SCM_EOL;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);

      Font_metric *fm = unsmob<Font_metric> (entry);

      if (dynamic_cast<Modified_font_metric *> (fm)
          || dynamic_cast<Pango_font *> (fm))
        font_list = scm_cons (fm->self_scm (), font_list);
    }

  return font_list;
}
