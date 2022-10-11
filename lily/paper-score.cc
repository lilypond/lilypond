/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "paper-score.hh"

#include "all-font-metrics.hh"
#include "book.hh"
#include "international.hh"
#include "misc.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"
#include "constrained-breaking.hh"

using std::vector;

Paper_score::Paper_score (Output_def *layout)
{
  layout_ = layout;
  system_ = 0;
  paper_systems_ = SCM_BOOL_F;
}

void
Paper_score::derived_mark () const
{
  if (system_)
    scm_gc_mark (system_->self_scm ());
  if (layout_)
    scm_gc_mark (layout_->self_scm ());
  scm_gc_mark (paper_systems_);
}

void
Paper_score::typeset_system (System *system)
{
  if (!system_)
    {
      system_ = system;
      system_->unprotect ();
    }

  system->pscore_ = this;
  system->set_layout (layout_);
}

void
Paper_score::find_break_indices () const
{
  cols_ = root_system ()->used_columns ();
  break_indices_.clear ();
  break_ranks_.clear ();

  for (vsize i = 0; i < cols_.size (); i++)
    {
      Paper_column *it = cols_[i];
      if (Paper_column::is_breakable (cols_[i])
          && (i == 0 || it->find_prebroken_piece (LEFT))
          && (i == cols_.size () - 1 || it->find_prebroken_piece (RIGHT)))
        {
          break_indices_.push_back (i);
          break_ranks_.push_back (it->get_column ()->get_rank ());
        }
    }
}

vector<vsize> const &
Paper_score::get_break_indices () const
{
  if (break_indices_.empty ())
    find_break_indices ();
  return break_indices_;
}

vector<Paper_column *> const &
Paper_score::get_columns () const
{
  if (cols_.empty ())
    find_break_indices ();
  return cols_;
}

vector<vsize> const &
Paper_score::get_break_ranks () const
{
  if (break_ranks_.empty ())
    find_break_indices ();
  return break_ranks_;
}

vector<Column_x_positions>
Paper_score::calc_breaking ()
{
  Constrained_breaking algorithm (this);
  vector<Column_x_positions> sol;

  message (_ ("Calculating line breaks...") + " ");

  int system_count = from_scm (layout ()->c_variable ("system-count"), 0);
  if (system_count)
    return algorithm.solve (0, VPOS, system_count);

  return algorithm.best_solution (0, VPOS);
}

void
Paper_score::process ()
{
  debug_output (_f ("Element count %zu (spanners %zu) ",
                    system_->element_count (), system_->spanner_count ()));

  message (_ ("Preprocessing graphical objects..."));

  system_->pre_processing ();
}

System *
Paper_score::root_system () const
{
  return system_;
}

Output_def *
Paper_score::layout () const
{
  return layout_;
}

SCM
Paper_score::get_paper_systems ()
{
  if (scm_is_false (paper_systems_))
    {
      vector<Column_x_positions> breaking = calc_breaking ();
      system_->break_into_pieces (breaking);
      message (_ ("Drawing systems...") + " ");
      system_->do_break_substitution_and_fixup_refpoints ();
      paper_systems_ = system_->get_paper_systems ();
    }
  return paper_systems_;
}
