/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context-def.hh"
#include "file-path.hh"
#include "global-context.hh"
#include "international.hh"
#include "interval.hh"
#include "ly-module.hh"
#include "output-def.hh"
#include "scm-hash.hh"
#include "warn.hh"

#include "program-option.hh"

#include "string-convert.hh"

using std::string;

Output_def::Output_def ()
{
  scope_ = SCM_EOL;
  parent_ = 0;

  smobify_self ();

  scope_ = ly_make_module ();
}

Output_def::Output_def (Output_def const &s)
  : Smob<Output_def> ()
{
  scope_ = SCM_EOL;
  parent_ = 0;
  smobify_self ();

  input_origin_ = s.input_origin_;
  scope_ = ly_make_module ();
  if (ly_is_module (s.scope_))
    ly_module_copy (scope_, s.scope_);
}

Output_def::~Output_def ()
{
}

SCM
Output_def::mark_smob () const
{
  /* FIXME: why is this necessary?
     all paper_ should be protected by themselves. */
  if (parent_)
    scm_gc_mark (parent_->self_scm ());

  return scope_;
}

void
assign_context_def (Output_def *m, SCM transdef)
{
  Context_def *tp = unsmob<Context_def> (transdef);
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
  Context_def *cd = unsmob<Context_def> (m->lookup_variable (name));
  return cd ? cd->self_scm () : SCM_EOL;
}

int
Output_def::print_smob (SCM p, scm_print_state *) const
{
  scm_puts ("#< ", p);
  scm_puts (class_name (), p);
  scm_puts (">", p);
  return 1;
}

Real
Output_def::get_dimension (SCM s) const
{
  SCM val = lookup_variable (s);
  return from_scm<double> (val);
}

SCM
Output_def::lookup_variable (SCM sym) const
{
  SCM var = scm_module_variable (scope_, sym);
  if (SCM_VARIABLEP (var) && !SCM_UNBNDP (SCM_VARIABLE_REF (var)))
    return SCM_VARIABLE_REF (var);

  if (parent_)
    return parent_->lookup_variable (sym);

  return SCM_UNDEFINED;
}

SCM
Output_def::c_variable (const string &s) const
{
  return lookup_variable (ly_symbol2scm (s));
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

  bool twosided = from_scm<bool> (c_variable ("two-sided"));
  // We don't distinguish between outer-margin / left-margin and so on
  // until page-stencil positioning in page.scm
  Real left_margin, left_margin_default;
  SCM scm_left_margin_default
    = (twosided ? c_variable ("outer-margin-default-scaled")
                : c_variable ("left-margin-default-scaled"));
  SCM scm_left_margin
    = (twosided ? c_variable ("outer-margin") : c_variable ("left-margin"));

  Real right_margin, right_margin_default;
  SCM scm_right_margin_default
    = (twosided ? c_variable ("inner-margin-default-scaled")
                : c_variable ("right-margin-default-scaled"));
  SCM scm_right_margin
    = (twosided ? c_variable ("inner-margin") : c_variable ("right-margin"));

  if (SCM_UNBNDP (scm_paper_width) || SCM_UNBNDP (scm_left_margin_default)
      || SCM_UNBNDP (scm_right_margin_default))
    {
      programming_error ("called normalize () on paper with missing settings");
      return;
    }
  else
    {
      paper_width = from_scm<double> (scm_paper_width);
      left_margin_default = from_scm<double> (scm_left_margin_default);
      right_margin_default = from_scm<double> (scm_right_margin_default);
    }

  Real line_width;
  Real line_width_default
    = paper_width - left_margin_default - right_margin_default;
  SCM scm_line_width = c_variable ("line-width");

  Real binding_offset = 0;
  if (twosided)
    binding_offset = from_scm<double> (c_variable ("binding-offset"), 0);

  if (SCM_UNBNDP (scm_line_width))
    {
      left_margin
        = (SCM_UNBNDP (scm_left_margin) ? left_margin_default
                                        : from_scm<double> (scm_left_margin));
      right_margin
        = (SCM_UNBNDP (scm_right_margin) ? right_margin_default
                                         : from_scm<double> (scm_right_margin))
          + binding_offset;
      line_width = paper_width - left_margin - right_margin;
    }
  else
    {
      line_width = from_scm<double> (scm_line_width);
      if (SCM_UNBNDP (scm_left_margin))
        {
          // Vertically center systems if only line-width is given
          if (SCM_UNBNDP (scm_right_margin))
            {
              left_margin = (paper_width - line_width) / 2;
              right_margin = left_margin;
            }
          else
            {
              right_margin
                = from_scm<double> (scm_right_margin) + binding_offset;
              left_margin = paper_width - line_width - right_margin;
            }
        }
      else
        {
          left_margin = from_scm<double> (scm_left_margin);
          right_margin = (SCM_UNBNDP (scm_right_margin)
                            ? (paper_width - line_width - left_margin)
                            : from_scm<double> (scm_right_margin))
                         + binding_offset;
        }
    }

  if (from_scm<bool> (c_variable ("check-consistency")))
    {
      // Consistency checks. If values don't match, set defaults.
      if (abs (paper_width - line_width - left_margin - right_margin) > 1e-6)
        {
          line_width = line_width_default;
          left_margin = left_margin_default;
          right_margin = right_margin_default;
          warning (
            _ ("margins do not fit with line-width, setting default values"));
        }
      else if ((left_margin < 0) || (right_margin < 0))
        {
          line_width = line_width_default;
          left_margin = left_margin_default;
          right_margin = right_margin_default;
          warning (_ ("systems run off the page due to improper paper "
                      "settings, setting default values"));
        }
    }

  set_variable (ly_symbol2scm ("left-margin"), to_scm (left_margin));
  set_variable (ly_symbol2scm ("right-margin"), to_scm (right_margin));
  set_variable (ly_symbol2scm ("line-width"), to_scm (line_width));
}

/* FIXME.  This is broken until we have a generic way of
   putting lists inside the \layout block.  */
Interval
line_dimension_interval (Output_def *def, vsize n)
{
  Real lw = def->get_dimension (ly_symbol2scm ("line-width"));
  Real ind = n ? def->get_dimension (ly_symbol2scm ("short-indent"))
               : def->get_dimension (ly_symbol2scm ("indent"));
  return Interval (ind, lw);
}
