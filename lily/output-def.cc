/*
  music-output-def.cc -- implement Output_def

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "output-def.hh"

#include "context-def.hh"
#include "file-path.hh"
#include "global-context.hh"
#include "international.hh"
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

void
Output_def::normalize ()
{
  Real paper_width;
  SCM scm_paper_width = c_variable ("paper-width");

  Real left_margin, left_margin_default;
  SCM scm_left_margin_default = c_variable ("left-margin-default-scaled");
  SCM scm_left_margin = c_variable ("left-margin");

  Real right_margin, right_margin_default;
  SCM scm_right_margin_default = c_variable ("right-margin-default-scaled");
  SCM scm_right_margin = c_variable ("right-margin");

  if (scm_paper_width == SCM_UNDEFINED
      || scm_left_margin_default == SCM_UNDEFINED
      || scm_right_margin_default == SCM_UNDEFINED)
    {
      programming_error ("called normalize () on paper with missing settings");
      return;
    }
  else
    {
      paper_width = scm_to_double (scm_paper_width);
      left_margin_default = scm_to_double (scm_left_margin_default);
      right_margin_default = scm_to_double (scm_right_margin_default);
    }

  Real line_width;
  Real line_width_default
    = paper_width - left_margin_default - right_margin_default;
  SCM scm_line_width = c_variable ("line-width");

  if (scm_line_width == SCM_UNDEFINED)
    {
      left_margin = ((scm_left_margin == SCM_UNDEFINED)
		     ? left_margin_default
		     : scm_to_double (scm_left_margin));
      right_margin = ((scm_right_margin == SCM_UNDEFINED)
		      ? right_margin_default
		      : scm_to_double (scm_right_margin));
      line_width = paper_width - left_margin - right_margin;
    }
  else
    {
      line_width = scm_to_double (scm_line_width);
      if (scm_left_margin == SCM_UNDEFINED)
        {
	  // Vertically center systems if only line-width is given
	  if (scm_right_margin == SCM_UNDEFINED)
            {
              left_margin = (paper_width - line_width) / 2;
              right_margin = left_margin;
            }
          else
            {
              right_margin = scm_to_double (scm_right_margin);
              left_margin = paper_width - line_width - right_margin;
            }
        }
      else
        {
          left_margin = scm_to_double (scm_left_margin);
          right_margin = ((scm_right_margin == SCM_UNDEFINED)
                           ? (paper_width - line_width - left_margin)
                           : scm_to_double (scm_right_margin));
        }
    }

  if (to_boolean (c_variable ("check-consistency")))
    {
      // Consistency checks. If values don't match, set defaults.
      if (abs (paper_width - line_width - left_margin - right_margin) > 1e-6)
        {
          line_width = line_width_default;
          left_margin = left_margin_default;
          right_margin = right_margin_default;
          warning (_ ("margins do not fit with line-width, setting default values"));
        }
      else if ((left_margin < 0) || (right_margin < 0))
        {
          line_width = line_width_default;
          left_margin = left_margin_default;
          right_margin = right_margin_default;
          warning (_ ("systems run off the page due to improper paper settings, setting default values"));
        }
    }

  set_variable (ly_symbol2scm ("left-margin"), scm_from_double (left_margin));
  set_variable (ly_symbol2scm ("right-margin"), scm_from_double (right_margin));
  set_variable (ly_symbol2scm ("line-width"), scm_from_double (line_width));
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


