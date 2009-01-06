/*
  paper-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-score.hh"

#include "all-font-metrics.hh"
#include "book.hh"
#include "international.hh"
#include "main.hh"
#include "misc.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-column.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"
#include "constrained-breaking.hh"

Paper_score::Paper_score (Output_def *layout)
{
  layout_ = layout;
  system_ = 0;
  systems_ = SCM_EOL;
  paper_systems_ = SCM_BOOL_F;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

void
Paper_score::derived_mark () const
{
  if (layout_)
    scm_gc_mark (layout_->self_scm ());
  scm_gc_mark (systems_);
  scm_gc_mark (paper_systems_);
}

void
Paper_score::typeset_system (System *system)
{
  if (!system_)
    system_ = system;

  systems_ = scm_cons (system->self_scm (), systems_);
  system->pscore_ = this;
  system->layout_ = layout_;
  system->unprotect ();
}


vector<vsize>
Paper_score::find_break_indices () const
{
  vector<Grob*> all = root_system ()->used_columns ();
  vector<vsize> retval;

  for (vsize i = 0; i < all.size (); i++)
    {
      Item *it = dynamic_cast<Item*> (all[i]);
      if (Paper_column::is_breakable (all[i])
	  && (i == 0 || it->find_prebroken_piece (LEFT))
	  && (i == all.size () - 1 || it->find_prebroken_piece (RIGHT)))
	retval.push_back (i);
    }

  cols_ = all;
  break_indices_ = retval;

  return retval;
}

vector<vsize>
Paper_score::get_break_indices () const
{
  if (break_indices_.empty ())
    find_break_indices ();
  return break_indices_;
}

vector<Grob*>
Paper_score::get_columns () const
{
  if (cols_.empty ())
    find_break_indices ();
  return cols_;
}

vector<Column_x_positions>
Paper_score::calc_breaking ()
{
  Constrained_breaking algorithm (this);
  vector<Column_x_positions> sol;

  message (_ ("Calculating line breaks...") + " ");

  int system_count = robust_scm2int (layout ()->c_variable ("system-count"), 0);
  if (system_count)
    return algorithm.solve (0, VPOS, system_count);

  return algorithm.best_solution (0, VPOS);
}

void
Paper_score::process ()
{
  if (be_verbose_global)
    message (_f ("Element count %d (spanners %d) ",
		 system_->element_count (),
		 system_->spanner_count ()));

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
  if (paper_systems_ == SCM_BOOL_F)
    {
      vector<Column_x_positions> breaking = calc_breaking ();
      system_->break_into_pieces (breaking);
      message (_ ("Drawing systems...") + " ");
      system_->do_break_substitution_and_fixup_refpoints ();
      paper_systems_ = system_->get_paper_systems ();
    }
  return paper_systems_;
}


Paper_score *
unsmob_paper_score (SCM x)
{
  return dynamic_cast<Paper_score*> (unsmob_music_output (x));
}
